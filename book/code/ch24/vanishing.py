"""Gradient size per layer in a deep network at initialization (Chapter 24).

A 10-layer network, 64 units per layer, random inputs.  We backpropagate a unit
error from the output and record the mean |dE/dW| of every layer.
"""
import numpy as np

from activations import ACT


def grad_per_layer(act, L=10, n=64, N=256, seed=0):
    rng = np.random.default_rng(seed)
    f, df = ACT[act]
    gain = 2.0 if "ReLU" in act else 1.0          # He init for ReLU, else Xavier/LeCun
    Ws = [rng.normal(0, np.sqrt(gain / n), (n, n)) for _ in range(L)]
    x = rng.normal(0, 1, (N, n))
    hs, ps = [x], []
    for W in Ws:                                  # forward
        ps.append(hs[-1] @ W); hs.append(f(ps[-1]))
    delta = np.ones_like(hs[-1]) / N              # dE/dh at the output
    g = []
    for k in reversed(range(L)):                  # backward
        delta = delta * df(ps[k])                 # through the activation
        g.append(np.abs(hs[k].T @ delta).mean())  # |dE/dW_k|
        delta = delta @ Ws[k].T                   # through the weights
    return np.array(g[::-1])                      # layer 1 first


if __name__ == "__main__":
    gs, gr = grad_per_layer("sigmoid"), grad_per_layer("ReLU")
    print("sigmoid first/last layer ratio %.1e" % (gs[0] / gs[-1]))
    print("ReLU    first/last layer ratio %.1e" % (gr[0] / gr[-1]))
    print("PASS" if gs[0] / gs[-1] < 1e-3 and gr[0] / gr[-1] > 0.1 else "FAIL")
