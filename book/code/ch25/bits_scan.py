"""Choose the total number of bits W of ap_fixed<W,I> for the toy network on the
VALIDATION set, then measure the chosen format once on the TEST set
(Chapter 25, Exercise 5; deck core exercise 3). NumPy emulation with fixed_point.quantize, not hls4ml.

Run:  python3 bits_scan.py path/to/toymc_0.npy      (writes auc_vs_bits.png, prints PASS)
"""
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from toy_mlp import load3, train, forward, roc, auc  # noqa: E402

(Xtr, ttr), (Xva, tva), (Xte, tte) = load3(sys.argv[1] if len(sys.argv) > 1 else "toymc_0.npy")
print(f"train {len(ttr)}, validation {len(tva)}, test {len(tte)} events")
s = 1 / 64                                   # inputs 0..50 -> 0..0.78
p = train(Xtr * s, ttr)
a_float = auc(*roc(tva, forward(p, Xva * s)))
print(f"float validation AUC {a_float:.4f}")
Ws = range(6, 19, 2)
best = {}
for I in (4, 6, 8):
    a = [auc(*roc(tva, forward(p, Xva * s, fmt=(W, I)))) for W in Ws]
    print(f"I = {I}:", " ".join(f"W={W}:{x:.4f}" for W, x in zip(Ws, a)))
    plt.plot(list(Ws), a, "o-", label=f"I = {I}")
    ok = [W for W, x in zip(Ws, a) if x > a_float - 0.001]
    best[I] = ok[0] if ok else None
plt.xlabel("total bits W"); plt.ylabel("validation AUC"); plt.legend()
plt.savefig("auc_vs_bits.png")
print("smallest W within 0.001 of float (validation):", best)
# the choice is made: look at the test set once, for the chosen format only
W = best[6]
t_float = auc(*roc(tte, forward(p, Xte * s)))
t_fixed = auc(*roc(tte, forward(p, Xte * s, fmt=(W, 6))))
print(f"chosen ap_fixed<{W},6>: test AUC {t_fixed:.4f}  (float {t_float:.4f})")
print("PASS" if best[6] == 12 and best[4] is None and abs(t_fixed - t_float) < 0.002
      else "FAIL")
