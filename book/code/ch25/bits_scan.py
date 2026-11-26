"""Test AUC of the toy network against the total number of bits W of ap_fixed<W,I>
(Chapter 25, core exercise 3). NumPy emulation with fixed_point.quantize, not hls4ml.

Run:  python3 bits_scan.py path/to/toymc_0.npy      (writes auc_vs_bits.png, prints PASS)
"""
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from toy_mlp import load, train, forward, roc, auc  # noqa: E402

(Xtr, ttr), (Xte, tte) = load(sys.argv[1] if len(sys.argv) > 1 else "toymc_0.npy")
s = 1 / 64                                   # inputs 0..50 -> 0..0.78
p = train(Xtr * s, ttr)
a_float = auc(*roc(tte, forward(p, Xte * s)))
print(f"float AUC {a_float:.4f}")
Ws = range(6, 19, 2)
best = {}
for I in (4, 6, 8):
    a = [auc(*roc(tte, forward(p, Xte * s, fmt=(W, I)))) for W in Ws]
    print(f"I = {I}:", " ".join(f"W={W}:{x:.4f}" for W, x in zip(Ws, a)))
    plt.plot(list(Ws), a, "o-", label=f"I = {I}")
    ok = [W for W, x in zip(Ws, a) if x > a_float - 0.001]
    best[I] = ok[0] if ok else None
plt.xlabel("total bits W"); plt.ylabel("test AUC"); plt.legend()
plt.savefig("auc_vs_bits.png")
print("smallest W within 0.001 of float:", best)
print("PASS" if best[6] == 12 and best[4] is None else "FAIL")
