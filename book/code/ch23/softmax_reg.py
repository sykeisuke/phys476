"""Multi-class (softmax) logistic regression with mini-batch SGD (Chapter 23).

Run:  python3 softmax_reg.py      prints the confusion matrix and PASS
"""
import numpy as np


def softmax(Z):
    Z = Z - Z.max(axis=1, keepdims=True)  # no overflow
    E = np.exp(Z)
    return E / E.sum(axis=1, keepdims=True)


def make_blobs3(n=300, seed=0):
    rng = np.random.default_rng(seed)
    centers = np.array([[0.0, 10.0], [5.0, 4.0], [10.0, 0.0]])
    X = np.vstack([rng.normal(c, 1.0, (n // 3, 2)) for c in centers])
    t = np.repeat(np.arange(3), n // 3)
    return X, t


def train_softmax(X, t, K=3, eta=0.05, B=32, epochs=100, seed=0):
    rng = np.random.default_rng(seed)
    T = np.eye(K)[t]                             # one-hot targets (1-of-K)
    W, b = np.zeros((X.shape[1], K)), np.zeros(K)
    for _ in range(epochs):
        order = rng.permutation(len(t))
        for s in range(0, len(t), B):
            i = order[s:s + B]
            Y = softmax(X[i] @ W + b)
            G = (Y - T[i]) / len(i)              # dE/dz = y - t
            W -= eta * X[i].T @ G
            b -= eta * G.sum(axis=0)
    return W, b


def confusion_matrix(t, y, K):
    C = np.zeros((K, K), dtype=int)
    for ti, yi in zip(t, y):
        C[ti, yi] += 1                           # row: true class, column: predicted
    return C


if __name__ == "__main__":
    X, t = make_blobs3()
    W, b = train_softmax(X, t)
    y = softmax(X @ W + b).argmax(axis=1)        # predicted class = argmax
    C = confusion_matrix(t, y, 3)
    print(C)
    print("PASS" if np.trace(C) / len(t) > 0.95 else "FAIL")
