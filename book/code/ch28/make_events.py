"""Write simulated muon decays as a text file for a Verilog testbench (Chapter 28, Project 1).

Each line: start time and stop time in ns (integers), separated by a space.
Decay times are exponential with tau = 2197 ns.
Each start comes GAP ns after the previous stop, so events never overlap and a simple
TDC (one start, one stop) can measure every event.
Run:  python3 make_events.py            -> writes events.txt and prints a summary
      python3 make_events.py --overlap  -> also writes events_overlap.txt (extension):
                                           the same decays with starts a fixed 10 us apart,
                                           so a few events start before the previous stop
"""
import sys

import numpy as np

GAP = 1000                                   # ns from a stop to the next start

rng = np.random.default_rng(1)
n, tau = 1000, 2197.0
dt = np.rint(rng.exponential(tau, n)).astype(int)
dt = np.maximum(dt, 1)                       # at least 1 ns

# start[i] = stop[i-1] + GAP
start = np.empty(n, dtype=int)
start[0] = 1000
start[1:] = 1000 + np.cumsum(dt[:-1] + GAP)
np.savetxt("events.txt", np.c_[start, start + dt], fmt="%d")
print(f"wrote events.txt: {n} events, mean decay time {dt.mean():.1f} ns, sum {dt.sum()}")
print(f"  longest decay {dt.max()} ns, gap stop -> next start {GAP} ns, "
      f"last stop at {start[-1] + dt[-1]} ns")

if "--overlap" in sys.argv:
    # extension: fixed 10 us start spacing; count events whose start comes before the
    # previous stop (a one-hit TDC loses or mismeasures them)
    start_o = 1000 + 10000 * np.arange(n)
    stop_o = start_o + dt
    n_ovl = int(np.sum(start_o[1:] < stop_o[:-1]))
    np.savetxt("events_overlap.txt", np.c_[start_o, stop_o], fmt="%d")
    print(f"wrote events_overlap.txt: starts 10000 ns apart, "
          f"{n_ovl} events start before the previous stop")
