"""Train the 3-16-32-32-1 toy-data network of Chapter 27 (the network of the repo notebook
hls4ml_1.ipynb, with the 2026 data bugs fixed) and save it for convert.py.

Same layers, names, L1 = 1e-4, lecun_uniform, Adam(1e-4), batch 1024 and 30 epochs as
hls4ml_1.ipynb, but:
  - only toymc_0.npy is used (toymc_1.npy is an identical copy),
  - the file is sorted (signal first), so it is shuffled and split 60/20/20 here
    (Keras validation_split would take the last, background-only, quarter),
  - the inputs (0..50) are divided by 64 so that they fit ap_fixed<16,6>.
The split is the same as book/code/ch24/load_toymc.py (default_rng(0) permutation).

Environment: the course conda environment 'hls4ml' (repo hls4ml/environment.yml).
Tested with Python 3.10.16, TensorFlow/Keras 2.11.0, hls4ml 1.1.0 (Apple-silicon Mac).
Run:  python3 train_toymc.py path/to/toymc_0.npy      -> writes toymc_model.h5
"""
import sys

import numpy as np

SCALE = 64.0           # inputs 0..50 -> 0..0.78 (power of 2: a free shift on an FPGA)
MODEL_FILE = "toymc_model.h5"   # HDF5: loads in Keras 2 and Keras 3


def load_split(path):
    """Return (X_tr, t_tr), (X_va, t_va), (X_te, t_te) with RAW inputs (not scaled)."""
    data = np.load(path)
    X, t = data[:, 0:3], data[:, 3]
    idx = np.random.default_rng(0).permutation(len(t))      # the file is sorted
    n1, n2 = int(0.6 * len(t)), int(0.8 * len(t))
    tr, va, te = idx[:n1], idx[n1:n2], idx[n2:]
    return (X[tr], t[tr]), (X[va], t[va]), (X[te], t[te])


def auc(t, y):
    """Area under the ROC curve = P(score of a signal event > score of a background event);
    equal scores (frequent after fixed-point rounding) count one half."""
    _, inv, cnt = np.unique(y, return_inverse=True, return_counts=True)
    upper = np.cumsum(cnt)
    ranks = (upper - (cnt - 1) / 2)[inv]          # average rank of each group of equal scores
    n_s, n_b = (t == 1).sum(), (t == 0).sum()
    return (ranks[t == 1].sum() - n_s * (n_s + 1) / 2) / (n_s * n_b)


def build_model():
    from tensorflow.keras.layers import Activation, Dense
    from tensorflow.keras.models import Sequential
    from tensorflow.keras.regularizers import l1
    kw = dict(kernel_initializer="lecun_uniform", kernel_regularizer=l1(0.0001))
    return Sequential([
        Dense(16, input_shape=(3,), name="fc1", **kw), Activation("relu", name="relu1"),
        Dense(32, name="fc2", **kw), Activation("relu", name="relu2"),
        Dense(32, name="fc3", **kw), Activation("relu", name="relu3"),
        Dense(1, name="output", **kw), Activation("sigmoid", name="sigmoid"),
    ])


if __name__ == "__main__":
    import tensorflow as tf
    path = sys.argv[1] if len(sys.argv) > 1 else "toymc_0.npy"
    np.random.seed(0)
    tf.random.set_seed(0)
    (X_tr, t_tr), (X_va, t_va), _ = load_split(path)
    model = build_model()
    print("parameters", model.count_params())                    # 1697
    model.compile(optimizer=tf.keras.optimizers.Adam(learning_rate=0.0001),
                  loss="binary_crossentropy", metrics=["accuracy"])
    model.fit(X_tr / SCALE, t_tr, batch_size=1024, epochs=30, verbose=2,
              validation_data=(X_va / SCALE, t_va))
    a = auc(t_va, model.predict(X_va / SCALE, verbose=0).ravel())
    print(f"validation AUC {a:.4f}")
    model.save(MODEL_FILE)
    print("saved", MODEL_FILE)
    print("PASS" if model.count_params() == 1697 and a > 0.98 else "FAIL")
