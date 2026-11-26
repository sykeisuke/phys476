"""Write simulated muon decays as a text file for a Verilog testbench (Chapter 28, Project 1).

Each line: start time and stop time in ns (integers), separated by a space.
Decay times are exponential with tau = 2197 ns; events are 10 us apart.
Run:  python3 make_events.py      -> writes events.txt and prints a summary
"""
import numpy as np

rng = np.random.default_rng(1)
n, tau = 1000, 2197.0
dt = np.rint(rng.exponential(tau, n)).astype(int)
dt = np.maximum(dt, 1)                       # at least 1 ns
start = 1000 + 10000 * np.arange(n)
np.savetxt("events.txt", np.c_[start, start + dt], fmt="%d")
print(f"wrote events.txt: {n} events, mean decay time {dt.mean():.1f} ns, sum {dt.sum()}")
