"""Keras -> hls4ml for the 3-16-32-32-1 toy-data network (Chapter 27).

Track A (any laptop, needs a C++ compiler, no AMD tools): convert, compile, predict, compare.
Track B (Vitis HLS on Linux/Windows): track_b() builds the project and reads the report.

Run train_toymc.py first (it writes toymc_model.h5), then, in the 'hls4ml' environment:
  python3 convert.py path/to/toymc_0.npy          # scaled inputs (/64): Keras and hls4ml agree
  python3 convert.py path/to/toymc_0.npy --raw    # raw 0..50 inputs in ap_fixed<16,6>: they wrap
  add --save out.npz to keep t, y_keras, y_hls for your own plots
Tested with Python 3.10.16, TensorFlow/Keras 2.11.0, hls4ml 1.1.0 (Apple-silicon Mac).
"""
import sys

import numpy as np
import hls4ml

from train_toymc import MODEL_FILE, SCALE, auc, load_split


def convert(model, out_dir="model_1/hls4ml_prj", reuse=1, strategy="Latency"):
    config = hls4ml.utils.config_from_keras_model(model, granularity="name")
    config["Model"]["Precision"] = "ap_fixed<16,6>"
    config["Model"]["ReuseFactor"] = reuse
    config["Model"]["Strategy"] = strategy
    hls_model = hls4ml.converters.convert_from_keras_model(
        model, hls_config=config, output_dir=out_dir,
        part="xc7a200tsbg484-1", backend="Vitis", io_type="io_parallel")
    return config, hls_model


def fix_ap_types_macos(hls_model):
    """Workaround (macOS, Apple clang 17 and newer): the ap_types headers shipped with
    hls4ml 1.1.0 forward-declare std::complex, which recent libc++ rejects ("reference to
    'complex' is ambiguous"). Replace the declaration by #include <complex>.
    Only touches the copy of the headers inside the generated project. Check whether a
    newer hls4ml release still needs it."""
    import pathlib
    fw = pathlib.Path(hls_model.config.get_output_dir()) / "firmware" / "ap_types"
    old = "namespace std {\ntemplate<typename _Tp> class complex;\n}"
    for name in ("ap_int_special.h", "ap_fixed_special.h"):
        f = fw / name
        f.write_text(f.read_text().replace(old, "#include <complex>"))


def compile_model(hls_model):
    """hls_model.compile(), with the macOS header workaround applied in between."""
    hls_model.write()                  # generate the C++ project
    if sys.platform == "darwin":
        fix_ap_types_macos(hls_model)
    hls_model._compile()               # build the shared library and load it


def track_a(model, hls_model, X_te_s):
    compile_model(hls_model)  # = compile() + macOS fix
    y_keras = model.predict(X_te_s, verbose=0).ravel()
    X_c = np.ascontiguousarray(X_te_s)
    y_hls = hls_model.predict(X_c).ravel()
    return y_keras, y_hls


def raw_input_model(model):
    """Same network for RAW inputs: divide the first-layer weights by 64 (Keras output unchanged)."""
    import tensorflow as tf
    m = tf.keras.models.clone_model(model)
    w = model.get_weights()
    w[0] = w[0] / SCALE
    m.set_weights(w)
    return m


def profile(model, hls_model, X):
    from hls4ml.model.profiling import numerical  # name may change between versions: check
    return numerical(model=model, hls_model=hls_model, X=X)


def track_b(hls_model, out_dir="model_1/hls4ml_prj"):
    hls_model.build(csim=False, synth=True)          # needs vitis_hls in PATH (Linux/Windows)
    hls4ml.report.read_vivado_report(out_dir)


if __name__ == "__main__":
    import tensorflow as tf
    path = next((a for a in sys.argv[1:] if not a.startswith("--")), "toymc_0.npy")
    raw = "--raw" in sys.argv
    save = sys.argv[sys.argv.index("--save") + 1] if "--save" in sys.argv else None
    if save:
        path = next(a for a in sys.argv[1:] if not a.startswith("--") and a != save)
    _, _, (X_te, t_te) = load_split(path)
    model = tf.keras.models.load_model(MODEL_FILE)
    if raw:
        model, X_in, out = raw_input_model(model), X_te, "model_1/hls4ml_prj_raw"
    else:
        X_in, out = X_te / SCALE, "model_1/hls4ml_prj"
    config, hls_model = convert(model, out_dir=out)
    y_keras, y_hls = track_a(model, hls_model, X_in)
    dd = np.abs(y_keras - y_hls)
    a_k, a_h = auc(t_te, y_keras), auc(t_te, y_hls)
    print(f"inputs {'raw 0..50' if raw else 'scaled /64'}, test set {len(t_te)} events")
    print(f"AUC keras {a_k:.4f}  AUC hls4ml {a_h:.4f}")
    print(f"|keras - hls|: median {np.median(dd):.4f}  99% below {np.quantile(dd, 0.99):.4f}"
          f"  max {dd.max():.4f}")
    if save:
        np.savez_compressed(save, t=t_te.astype(np.int8), y_keras=y_keras.astype(np.float32),
                            y_hls=y_hls.astype(np.float32))
    if raw:   # the point of this run is to SEE the wrap-around
        print("PASS (expected: hls4ml AUC clearly below Keras)" if a_h < a_k - 0.02 else "FAIL")
    else:
        print("PASS" if abs(a_k - a_h) < 0.002 else "FAIL")
