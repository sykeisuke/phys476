"""Extract baseline, amplitude and peak time from sampled pulses.

Use it on the ADC captures of the DAC-to-ADC loopback. Here the pulses are
simulated (1 ns sampling, noise) so that the script runs anywhere.
Run:  python3 pulse_features.py
"""
import numpy as np

rng = np.random.default_rng(1)
dt = 1.0                                     # ns per sample (example)
t = np.arange(100) * dt


def pulse(t, t0, amp, rise=1.5, decay=8.0):
    x = np.clip(t - t0, 0, None)
    return amp * (1 - np.exp(-x / rise)) * np.exp(-x / decay)


def features(y, n_base=20):
    base = y[:n_base].mean()                 # pedestal from the first samples
    k = int(np.argmax(y))
    a, b, c = y[k - 1], y[k], y[k + 1]       # parabola through the peak sample
    shift = 0.5 * (a - c) / (a - 2 * b + c)
    return base, b - base, (k + shift) * dt


for t0 in (30.0, 30.4, 55.7):
    y = 50 + pulse(t, t0, 400) + rng.normal(0, 3, t.size)  # ADC counts
    base, amp, tpk = features(y)
    print(f"t0 = {t0:5.1f} ns: baseline {base:5.1f}, amplitude {amp:5.1f}, "
          f"peak at {tpk:5.1f} ns")
