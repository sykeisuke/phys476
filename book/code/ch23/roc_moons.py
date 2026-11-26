"""ROC curve and AUC of the moons MLPs on the validation set (Exercise 9, Chapter 23).

Same code as the Week 13 lab helper, plus a PASS line.  Writes roc_moons.png.
Run:  python3 roc_moons.py
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt   # noqa: E402

from mlp import MLP, make_moons   # noqa: E402
from metrics import split, roc, auc   # noqa: E402

X, t = make_moons()
(Xtr, ttr), (Xva, tva), _ = split(X, t)
a = {}
for H in (2, 3):
    m = MLP(2, H).fit(Xtr, ttr[:, None], eta=0.5, epochs=300)
    fpr, tpr = roc(tva, m.forward(Xva).ravel())
    a[H] = auc(fpr, tpr)
    print(f"H = {H}: validation AUC = {a[H]:.4f}")
    plt.plot(fpr, tpr, label=f"H = {H}, AUC = {a[H]:.3f}")
plt.xlabel("false positive rate"); plt.ylabel("true positive rate"); plt.legend()
plt.savefig("roc_moons.png")
print("PASS" if 0.85 < a[2] < a[3] <= 1.0 else "FAIL")
