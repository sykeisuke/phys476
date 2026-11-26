"""Python minimum: a numpy array and a matplotlib plot of a 100 MHz clock (Chapter 0).

Run:  python3 clock_plot.py      -> writes clock_100MHz.png next to this script
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")            # draw into a file; no window needed
import matplotlib.pyplot as plt
import numpy as np

f = 100e6                        # clock frequency, Hz
T = 1 / f                        # period, s  (10 ns)
duty = 0.5                       # fraction of the period that the clock is high
V_high = 3.3                     # V

t = np.linspace(0, 4 * T, 2001)              # 4 periods, 2001 time points
phase = (t % T) / T                          # 0 .. 1 within each period
v = np.where(phase < duty, V_high, 0.0)      # high for the first half of each period

print(f"period T = {T * 1e9:.1f} ns, frequency f = {f / 1e6:.0f} MHz")
print(f"array t has {t.size} points, from {t[0]*1e9:.0f} to {t[-1]*1e9:.0f} ns")

fig, ax = plt.subplots(figsize=(7, 2.5))
ax.plot(t * 1e9, v, lw=2)
ax.set_xlabel("time (ns)")
ax.set_ylabel("voltage (V)")
ax.set_title("100 MHz clock: period 10 ns, duty cycle 50 %")
ax.set_ylim(-0.5, 4.0)
ax.grid(alpha=0.3)
fig.tight_layout()
out = Path(__file__).with_name("clock_100MHz.png")
fig.savefig(out, dpi=120)
print("wrote", out.name)
