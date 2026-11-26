"""A small multi-layer perceptron in NumPy (Chapter 23).

One hidden layer with tanh; output 'sigmoid' (binary classification, BCE loss)
or 'identity' (regression, MSE loss). Trained with mini-batch SGD.
Run:  python3 mlp.py      trains on the two-moons data and on a noisy sine; prints PASS
"""
import numpy as np


class MLP:
    def __init__(self, n_in, n_hidden, n_out=1, output="sigmoid", seed=0):
        rng = np.random.default_rng(seed)
        self.W = rng.normal(0, 1 / np.sqrt(n_in), (n_in, n_hidden))
        self.b = np.zeros(n_hidden)
        self.V = rng.normal(0, 1 / np.sqrt(n_hidden), (n_hidden, n_out))
        self.c = np.zeros(n_out)
        self.output = output

    def forward(self, X):
        self.X = X
        self.h = np.tanh(X @ self.W + self.b)             # hidden layer
        q = self.h @ self.V + self.c
        return 1 / (1 + np.exp(-q)) if self.output == "sigmoid" else q

    def step(self, X, T, eta):
        Y = self.forward(X)
        d_out = (Y - T) / len(X)          # dE/dq (both cases)
        d_hid = (d_out @ self.V.T) * (1 - self.h ** 2)   # tanh' = 1 - h^2
        self.V -= eta * self.h.T @ d_out
        self.c -= eta * d_out.sum(axis=0)
        self.W -= eta * X.T @ d_hid
        self.b -= eta * d_hid.sum(axis=0)

    def fit(self, X, T, eta=0.1, B=32, epochs=200, seed=0):
        rng = np.random.default_rng(seed)
        for _ in range(epochs):
            order = rng.permutation(len(X))
            for s in range(0, len(X), B):
                i = order[s:s + B]
                self.step(X[i], T[i], eta)
        return self


def make_moons(n=300, noise=0.2, seed=0):
    """Two interleaving half circles (like sklearn.datasets.make_moons)."""
    rng = np.random.default_rng(seed)
    a = rng.uniform(0, np.pi, n // 2)
    X0 = np.c_[np.cos(a), np.sin(a)]
    X1 = np.c_[1 - np.cos(a), 0.5 - np.sin(a)]
    X = np.vstack([X0, X1]) + rng.normal(0, noise, (n, 2))
    t = np.r_[np.zeros(n // 2), np.ones(n // 2)]
    return X, t


def make_sine(n=200, noise=0.2, seed=0):
    rng = np.random.default_rng(seed)
    x = rng.uniform(-3, 3, n)
    return x[:, None], (np.sin(x) + rng.normal(0, noise, n))[:, None]


if __name__ == "__main__":
    from metrics import split
    X, t = make_moons()
    (Xtr, ttr), (Xva, tva), _ = split(X, t)          # shuffled: the data are sorted
    acc = {}
    for H in (2, 3, 8):
        m = MLP(2, H).fit(Xtr, ttr[:, None], eta=0.5, epochs=300)
        acc[H] = round(float(np.mean((m.forward(Xva).ravel() > 0.5) == tva)), 3)
    print("moons validation accuracy for H = 2, 3, 8:", acc)
    x, y = make_sine()
    r = MLP(1, 10, output="identity")
    r.fit(x, y, eta=0.05, epochs=500)
    rmse = np.sqrt(np.mean((r.forward(x) - y) ** 2))
    print(f"sine fit RMSE {rmse:.3f} (noise sigma 0.2)")
    print("PASS" if max(acc.values()) > 0.9 and rmse < 0.3 else "FAIL")
