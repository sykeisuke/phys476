"""Perceptron trained with the error-correction rule (Chapter 23).

Run:  python3 perceptron.py      prints PASS when AND and OR are learned
      and XOR is not (no straight line separates XOR).
"""
import numpy as np


def step(z):
    return (z > 0).astype(int)          # 1 if z > 0, else 0


def train_perceptron(X, t, eta=1.0, epochs=20):
    """y = step(w.x + b).  For each sample: w += eta*(t-y)*x, b += eta*(t-y)."""
    w, b = np.zeros(X.shape[1]), 0.0
    for _ in range(epochs):
        errors = 0
        for x, tk in zip(X, t):
            y = step(w @ x + b)
            w += eta * (tk - y) * x
            b += eta * (tk - y)
            errors += int(tk != y)
        if errors == 0:                  # a full pass without mistakes: done
            return w, b, True
    return w, b, False


def trace(X, t, eta=1.0, epochs=3):
    """Rows (k, x1, x2, t, w1, w2, b, y, t-y) for the hand exercise, w = b = 0 at the start."""
    w, b, rows, k = np.zeros(X.shape[1]), 0.0, [], 0
    for _ in range(epochs):
        for x, tk in zip(X, t):
            y = int(step(w @ x + b))
            rows.append((k, *x.astype(int), tk, *w, b, y, tk - y))
            w += eta * (tk - y) * x
            b += eta * (tk - y)
            k += 1
    return rows


if __name__ == "__main__":
    X = np.array([[0, 0], [0, 1], [1, 0], [1, 1]], dtype=float)
    ok = True
    for name, t, separable in (("AND", [0, 0, 0, 1], True),
                               ("OR", [0, 1, 1, 1], True),
                               ("XOR", [0, 1, 1, 0], False)):
        w, b, converged = train_perceptron(X, np.array(t), epochs=100)
        print(f"{name}: w={w}, b={b}, converged={converged}")
        ok &= (converged == separable)
        if converged:
            ok &= bool(np.all(step(X @ w + b) == np.array(t)))
    print("PASS" if ok else "FAIL")
