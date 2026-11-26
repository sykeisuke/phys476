"""A 3-16-1 ReLU network trained in NumPy on the toy Monte Carlo data, then
evaluated in floating point and in emulated fixed point (Chapter 25).

This is an emulation with fixed_point.quantize, not hls4ml: it shows what
precision and input scaling do, not the exact hls4ml numbers.
Run:  python3 toy_mlp.py path/to/toymc_0.npy
"""
import sys

import numpy as np

from fixed_point import quantize

sys.path.insert(0, __file__.rsplit("/", 2)[0] + "/ch23")
from metrics import roc, auc  # noqa: E402


def sigmoid(z):
    return 1 / (1 + np.exp(-z))


def load(path, n=60000, seed=0):
    d = np.load(path)
    idx = np.random.default_rng(seed).permutation(len(d))[:n]
    X, t = d[idx, :3], d[idx, 3]
    return (X[: n // 2], t[: n // 2]), (X[n // 2:], t[n // 2:])


def train(X, t, H=16, eta=0.05, B=64, epochs=15, seed=1):
    """Plain mini-batch SGD (Chapter 23); X must already be scaled."""
    rng = np.random.default_rng(seed)
    W = rng.normal(0, np.sqrt(2 / 3), (3, H)); b = np.zeros(H)
    V = rng.normal(0, np.sqrt(2 / H), (H, 1)); c = np.zeros(1)
    T = t[:, None]
    for _ in range(epochs):
        order = rng.permutation(len(t))
        for s in range(0, len(t), B):
            i = order[s:s + B]
            p = X[i] @ W + b; h = np.maximum(p, 0)
            y = sigmoid(h @ V + c)
            d_out = (y - T[i]) / len(i)
            d_hid = (d_out @ V.T) * (p > 0)
            V -= eta * h.T @ d_out; c -= eta * d_out.sum(0)
            W -= eta * X[i].T @ d_hid; b -= eta * d_hid.sum(0)
    return W, b, V, c


def forward(params, X, fmt=None, x_fmt=None):
    """fmt = (W, I) quantizes weights and every result; x_fmt the inputs."""
    W, b, V, c = params
    q = (lambda a: a) if fmt is None else (lambda a: quantize(a, *fmt))
    qx = q if x_fmt is None else (lambda a: quantize(a, *x_fmt))
    h = q(np.maximum(q(qx(X) @ q(W) + q(b)), 0))
    return sigmoid(q(h @ q(V) + q(c))).ravel()


if __name__ == "__main__":
    path = sys.argv[1] if len(sys.argv) > 1 else "toymc_0.npy"
    (Xtr, ttr), (Xte, tte) = load(path)
    scale = 1 / 64                                   # 0..50 -> 0..0.78
    params = train(Xtr * scale, ttr)
    a_float = auc(*roc(tte, forward(params, Xte * scale)))
    a_16 = auc(*roc(tte, forward(params, Xte * scale, fmt=(16, 6))))
    a_6 = auc(*roc(tte, forward(params, Xte * scale, fmt=(6, 3))))
    # raw 0..50 inputs (weights rescaled so the float function is the same);
    # only the inputs are quantized, everything else stays float
    params_raw = (params[0] * scale,) + params[1:]
    a_wrap = auc(*roc(tte, forward(params_raw, Xte, x_fmt=(16, 6))))
    a_wide = auc(*roc(tte, forward(params_raw, Xte, x_fmt=(16, 7))))
    print(f"AUC float {a_float:.4f}  <16,6> {a_16:.4f}  <6,3> {a_6:.4f}")
    print(f"raw inputs in <16,6> (wrap) {a_wrap:.4f}   in <16,7> {a_wide:.4f}")
    ok = (a_float > 0.9 and abs(a_16 - a_float) < 0.005 and a_6 < a_float - 0.005
          and abs(a_wide - a_float) < 0.001 and a_wrap < a_float - 0.01)
    print("PASS" if ok else "FAIL")
