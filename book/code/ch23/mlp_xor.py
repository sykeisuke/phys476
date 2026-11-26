"""A 2-H-1 multi-layer perceptron learns XOR with backpropagation (Chapter 23).

Hidden layer: tanh.  Output: sigmoid.  Loss: binary cross-entropy.
Full-batch gradient descent (the data set has only 4 points, so B = N = 4).
"""
import numpy as np


def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))


def train_xor(H=3, eta=0.5, epochs=5000, seed=0):
    rng = np.random.default_rng(seed)
    X = np.array([[0, 0], [0, 1], [1, 0], [1, 1]], dtype=float)
    t = np.array([[0], [1], [1], [0]], dtype=float)
    W, b = rng.normal(0, 1, (2, H)), np.zeros(H)       # input -> hidden
    V, c = rng.normal(0, 1, (H, 1)), np.zeros(1)       # hidden -> output
    for _ in range(epochs):
        p = X @ W + b; h = np.tanh(p)       # forward
        q = h @ V + c; y = sigmoid(q)
        d_out = (y - t) / len(X)            # dE/dq
        # h = tanh(p), so dh/dp = tanh'(p) = 1 - h**2
        d_hid = (d_out @ V.T) * (1 - h**2)  # dE/dp
        V -= eta * h.T @ d_out
        c -= eta * d_out.sum(0)
        W -= eta * X.T @ d_hid
        b -= eta * d_hid.sum(0)
    return (W, b, V, c), X, t


def predict(params, X):
    W, b, V, c = params
    return sigmoid(np.tanh(X @ W + b) @ V + c)


if __name__ == "__main__":
    params, X, t = train_xor()
    y = predict(params, X)
    print(np.round(y.ravel(), 3))
    print("PASS" if np.all((y > 0.5) == (t > 0.5)) else "FAIL")
