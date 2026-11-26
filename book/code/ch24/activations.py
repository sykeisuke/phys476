"""Activation functions and their derivatives (Chapter 24)."""
import numpy as np


def sigmoid(z):
    return 1 / (1 + np.exp(-z))


ACT = {
    "sigmoid": (sigmoid, lambda z: sigmoid(z) * (1 - sigmoid(z))),
    "tanh": (np.tanh, lambda z: 1 - np.tanh(z) ** 2),
    "ReLU": (lambda z: np.maximum(0, z), lambda z: (z > 0).astype(float)),
    "leaky ReLU": (lambda z: np.where(z > 0, z, 0.01 * z),
                   lambda z: np.where(z > 0, 1.0, 0.01)),
}

if __name__ == "__main__":
    z = np.linspace(-6, 6, 120001)
    for name, (f, df) in ACT.items():
        print(f"{name:10s} max derivative {df(z).max():.4f}")
    ok = (abs(ACT["sigmoid"][1](z).max() - 0.25) < 1e-9
          and abs(ACT["tanh"][1](z).max() - 1.0) < 1e-9)
    # derivative check against finite differences away from the ReLU kink
    zz = np.array([-2.0, -0.5, 0.7, 3.0]); h = 1e-6
    for f, df in ACT.values():
        ok &= np.allclose((f(zz + h) - f(zz - h)) / (2 * h), df(zz), atol=1e-6)
    print("PASS" if ok else "FAIL")
