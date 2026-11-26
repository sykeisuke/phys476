"""Load the toy Monte Carlo sample and split it into train / validation / test.

Data: toymc_0.npy from the course repo (phys476/hls4ml/).  Columns 0-2 are three
input variables (each between 0 and 50), column 3 is the label (1 = signal,
0 = background).  Note: the file is sorted (all signal first), and toymc_1.npy
is an identical copy, so we use ONE file and split it after shuffling.

Run:  python3 load_toymc.py path/to/toymc_0.npy
"""
import sys

import numpy as np

path = sys.argv[1] if len(sys.argv) > 1 else 'toymc_0.npy'
data = np.load(path)          # straight quotes
X, t = data[:, 0:3], data[:, 3]

rng = np.random.default_rng(0)
idx = rng.permutation(len(t))  # file is sorted
n1, n2 = int(0.6 * len(t)), int(0.8 * len(t))
tr, va, te = idx[:n1], idx[n1:n2], idx[n2:]
X_tr, t_tr = X[tr], t[tr]
X_va, t_va = X[va], t[va]
X_te, t_te = X[te], t[te]

if __name__ == "__main__":
    print("shape", data.shape, " input range", X.min(), X.max())
    for name, tt in (("train", t_tr), ("val", t_va), ("test", t_te)):
        print(f"{name:5s} {len(tt):6d} events, signal fraction {tt.mean():.3f}")
    ok = (len(t_tr) + len(t_va) + len(t_te) == len(t)
          and all(abs(tt.mean() - t.mean()) < 0.02 for tt in (t_tr, t_va, t_te)))
    print("PASS" if ok else "FAIL")
