"""Train/validation/test split, confusion matrix and ROC/AUC (Chapter 23)."""
import numpy as np


def split(X, t, f_train=0.6, f_val=0.2, seed=0):
    """Shuffle, then cut into train/val/test."""
    rng = np.random.default_rng(seed)
    idx = rng.permutation(len(t))
    n1 = int(f_train * len(t))
    n2 = n1 + int(f_val * len(t))
    tr, va, te = idx[:n1], idx[n1:n2], idx[n2:]
    return ((X[tr], t[tr]), (X[va], t[va]),
            (X[te], t[te]))


def confusion(t, y):
    """t: true labels (0/1), y: predicted labels (0/1)."""
    TP = int(np.sum((t == 1) & (y == 1))); FN = int(np.sum((t == 1) & (y == 0)))
    FP = int(np.sum((t == 0) & (y == 1))); TN = int(np.sum((t == 0) & (y == 0)))
    return TP, FP, FN, TN


def roc(t, score):
    """TPR and FPR for every threshold (from high to low score)."""
    order = np.argsort(-score)
    ts = t[order]
    tpr = np.r_[0.0, np.cumsum(ts == 1) / np.sum(t == 1)]
    fpr = np.r_[0.0, np.cumsum(ts == 0) / np.sum(t == 0)]
    return fpr, tpr


def auc(fpr, tpr):
    return float(np.sum(np.diff(fpr) * (tpr[1:] + tpr[:-1]) / 2))   # trapezoid rule


if __name__ == "__main__":
    rng = np.random.default_rng(0)
    t = np.r_[np.ones(500), np.zeros(500)]
    score = np.r_[rng.normal(1.0, 1.0, 500), rng.normal(0.0, 1.0, 500)]
    TP, FP, FN, TN = confusion(t, (score > 0.5).astype(int))
    acc = (TP + TN) / len(t); prec = TP / (TP + FP); rec = TP / (TP + FN)
    fpr, tpr = roc(t, score)
    a = auc(fpr, tpr)
    # AUC = probability that a random signal scores above a random background
    p = np.mean(score[t == 1][:, None] > score[t == 0][None, :])
    (Xtr, _), (Xva, _), (Xte, _) = split(score[:, None], t)
    print(f"acc {acc:.3f} precision {prec:.3f} recall {rec:.3f} AUC {a:.4f} P(s>b) {p:.4f}")
    print(f"split sizes {len(Xtr)} {len(Xva)} {len(Xte)}")
    ok = abs(a - p) < 1e-9 and len(Xtr) + len(Xva) + len(Xte) == len(t)
    print("PASS" if ok else "FAIL")
