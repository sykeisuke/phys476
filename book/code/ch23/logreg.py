"""Logistic regression trained with mini-batch SGD (Chapter 23).

Definition used throughout the book:
  mini-batch SGD = shuffle the training set every epoch, cut it into batches of
  B samples, and update the parameters once per batch with the batch-averaged
  gradient.  B = 1 is per-sample (online) SGD; B = N is full-batch gradient descent.
"""
import numpy as np


def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))


def bce(y, t, eps=1e-12):
    """Binary cross-entropy (negative log-likelihood), averaged over samples."""
    y = np.clip(y, eps, 1 - eps)
    return float(np.mean(-(t * np.log(y) + (1 - t) * np.log(1 - y))))


def sgd_logreg(X, t, eta=0.1, B=32, epochs=50, seed=0):
    rng = np.random.default_rng(seed)
    N, d = X.shape
    w, b = np.zeros(d), 0.0
    history = []
    for _ in range(epochs):
        order = rng.permutation(N)      # shuffle
        for start in range(0, N, B):
            idx = order[start:start + B]  # batch
            y = sigmoid(X[idx] @ w + b)
            err = y - t[idx]              # dE/dz
            # one update with the batch-mean gradient
            w -= eta * X[idx].T @ err / len(idx)
            b -= eta * err.mean()
        history.append(bce(sigmoid(X @ w + b), t))
    return w, b, history


def make_blobs(n=400, sep=2.0, seed=1):
    """Two overlapping Gaussian classes in 2D (label 0 and 1)."""
    rng = np.random.default_rng(seed)
    x0 = rng.normal(0.0, 1.0, size=(n // 2, 2))
    x1 = rng.normal(sep, 1.0, size=(n // 2, 2))
    X = np.vstack([x0, x1])
    t = np.r_[np.zeros(n // 2), np.ones(n // 2)]
    return X, t


if __name__ == "__main__":
    X, t = make_blobs()
    # gradient check: analytic gradient of the mean BCE vs finite differences
    rng = np.random.default_rng(3)
    w0, b0 = rng.normal(size=2), 0.3
    g_an = X.T @ (sigmoid(X @ w0 + b0) - t) / len(t)
    h = 1e-6
    g_num = np.array([(bce(sigmoid(X @ (w0 + h * e) + b0), t) -
                       bce(sigmoid(X @ (w0 - h * e) + b0), t)) / (2 * h) for e in np.eye(2)])
    grad_ok = np.allclose(g_an, g_num, rtol=1e-4, atol=1e-7)
    w, b, hist = sgd_logreg(X, t, eta=0.1, B=32, epochs=50)
    acc = np.mean((sigmoid(X @ w + b) > 0.5) == t)
    print(f"gradient check {grad_ok}, final loss {hist[-1]:.3f}, accuracy {acc:.3f}")
    print("PASS" if grad_ok and hist[-1] < hist[0] and acc > 0.8 else "FAIL")
