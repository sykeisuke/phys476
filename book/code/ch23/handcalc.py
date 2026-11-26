"""One forward and one backward pass through a 2-2-1 network, by numbers (Chapter 23).

Hidden and output units use the sigmoid; loss = binary cross-entropy.
Run:  python3 handcalc.py      prints every intermediate number and PASS
"""
import numpy as np


def sigmoid(z):
    return 1 / (1 + np.exp(-z))


x = np.array([1.0, 0.0]); t = 1.0                    # one training sample
W = np.array([[0.5, -0.5],                            # W[i, j]: input i -> hidden j
              [0.3, 0.8]])
b = np.array([0.0, 0.0])
V = np.array([1.0, -1.0]); c = 0.0                    # hidden j -> output
eta = 0.5


def loss(W, b, V, c):
    h = sigmoid(x @ W + b)
    y = sigmoid(h @ V + c)
    return -(t * np.log(y) + (1 - t) * np.log(1 - y))


# forward
p = x @ W + b;        h = sigmoid(p)
q = h @ V + c;        y = sigmoid(q)
E = loss(W, b, V, c)
# backward
d_out = y - t                                         # dE/dq
dV = d_out * h;       dc = d_out
d_hid = d_out * V * h * (1 - h)                       # dE/dp_j
dW = np.outer(x, d_hid) + 0.0; db = d_hid

if __name__ == "__main__":
    np.set_printoptions(precision=4, suppress=True)
    print("p =", p, " h =", h)
    print(f"q = {q:.4f}  y = {y:.4f}  E = {E:.4f}")
    print(f"d_out = {d_out:.4f}  dV =", dV, " d_hid =", d_hid)
    print("dW =\n", dW)
    W2, V2 = W - eta * dW, V - eta * dV
    print("new V =", V2, f" new E = {loss(W2, b - eta * db, V2, c - eta * dc):.4f}")
    # check one gradient by finite differences
    eps = 1e-6; Wp = W.copy(); Wp[0, 0] += eps; Wm = W.copy(); Wm[0, 0] -= eps
    num = (loss(Wp, b, V, c) - loss(Wm, b, V, c)) / (2 * eps)
    print(f"dE/dW[0,0]: backprop {dW[0, 0]:.6f}  finite difference {num:.6f}")
    print("PASS" if abs(num - dW[0, 0]) < 1e-8 else "FAIL")
