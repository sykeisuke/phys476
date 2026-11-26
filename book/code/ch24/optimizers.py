"""SGD, momentum, RMSprop and Adam on a 2D loss surface (Chapter 24).

Loss: E(w) = 0.5 * (a*w1^2 + b*w2^2), a long narrow valley (a << b).
Each optimizer is a function that returns the next position.
"""
import numpy as np

A = np.array([0.1, 2.0])                 # curvatures along w1 and w2


def loss(w):
    return 0.5 * np.sum(A * w ** 2, axis=-1)


def grad(w):
    return A * w


def run(method, w0=(-4.0, 1.5), steps=60, eta=0.4, gamma=0.9,
        rho=0.9, b1=0.9, b2=0.999, eps=1e-8):
    w = np.array(w0, dtype=float)
    v = np.zeros(2); r = np.zeros(2); m = np.zeros(2)
    path = [w.copy()]
    for k in range(1, steps + 1):
        g = grad(w)
        if method == "SGD":
            w = w - eta * g
        elif method == "momentum":
            v = gamma * v - eta * g
            w = w + v
        elif method == "AdaGrad":
            r = r + g ** 2                     # sum of all squared gradients
            w = w - eta * g / (np.sqrt(r) + eps)
        elif method == "RMSprop":
            r = rho * r + (1 - rho) * g ** 2
            w = w - eta * g / (np.sqrt(r) + eps)
        elif method == "Adam":
            m = b1 * m + (1 - b1) * g
            r = b2 * r + (1 - b2) * g ** 2
            m_hat, r_hat = m / (1 - b1 ** k), r / (1 - b2 ** k)
            w = w - eta * m_hat / (np.sqrt(r_hat) + eps)
        path.append(w.copy())
    return np.array(path)


SETTINGS = {"SGD": dict(eta=0.9), "momentum": dict(eta=0.3), "AdaGrad": dict(eta=0.8),
            "RMSprop": dict(eta=0.1), "Adam": dict(eta=0.3)}

def adam_first_step(g=2.0, eta=0.001, b1=0.9, b2=0.999, eps=1e-8):
    """Worked example: the first Adam step for a single parameter with gradient g."""
    m = (1 - b1) * g; r = (1 - b2) * g ** 2
    m_hat, r_hat = m / (1 - b1), r / (1 - b2)
    return m, r, m_hat, r_hat, eta * m_hat / (np.sqrt(r_hat) + eps)


if __name__ == "__main__":
    print("Adam first step for g = 2: m, r, m_hat, r_hat, step =",
          [round(v, 6) for v in adam_first_step()])
    ok = abs(adam_first_step()[-1] - 0.001) < 1e-9
    for name, kw in SETTINGS.items():
        p = run(name, **kw)
        print(f"{name:9s} loss {loss(p[0]):.3f} -> {loss(p[-1]):.4f}")
        ok &= loss(p[-1]) < 0.2 * loss(p[0])
    print("PASS" if ok else "FAIL")
