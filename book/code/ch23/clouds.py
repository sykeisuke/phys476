"""Exercises 3 and 4 (Chapter 23): the perceptron on two Gaussian clouds of points.

Counts the epochs until a full pass has no error (that error-free pass is counted).
Default data = the figure of Exercise 3: 10 + 10 points, cloud centres 0 and 3, 2-D.
Run:  python3 clouds.py
"""
import numpy as np

from perceptron import step, train_perceptron


def clouds(n=10, dist=3.0, dim=2, seed=7):
    """n points around (0, ..., 0) with t = 0 and n points around (dist, ..., dist) with t = 1."""
    rng = np.random.default_rng(seed)
    X = np.vstack([rng.normal(0, 1, (n, dim)), rng.normal(dist, 1, (n, dim))])
    t = np.r_[np.zeros(n), np.ones(n)].astype(int)
    return X, t


def epochs_to_converge(X, t, eta=0.1, max_epochs=1000):
    """Same rule as train_perceptron; returns (epochs, w, b), epochs = None if no convergence."""
    w, b = np.zeros(X.shape[1]), 0.0
    for ep in range(1, max_epochs + 1):
        errors = 0
        for x, tk in zip(X, t):
            y = step(w @ x + b)
            w += eta * (tk - y) * x
            b += eta * (tk - y)
            errors += int(tk != y)
        if errors == 0:
            return ep, w, b
    return None, w, b


if __name__ == "__main__":
    for n, dist, dim in ((10, 3.0, 2), (100, 3.0, 2), (100, 6.0, 2), (10, 1.5, 2), (10, 3.0, 3)):
        X, t = clouds(n, dist, dim)
        ep, w, b = epochs_to_converge(X, t)
        print(f"N = {2 * n:3d}  distance {dist}  dim {dim}:  "
              + (f"converged after {ep} epochs" if ep else "no convergence in 1000 epochs"))
    X, t = clouds()
    ep, w, b = epochs_to_converge(X, t)
    w2, b2, ok = train_perceptron(X, t, eta=0.1, epochs=100)
    print("PASS" if ok and ep is not None and np.allclose(w, w2) and b == b2 else "FAIL")
