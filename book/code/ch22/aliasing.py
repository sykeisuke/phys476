"""Aliasing: sampled at fs, frequencies f, fs - f and fs + f give the same samples.

Run:  python3 aliasing.py
"""
import numpy as np

fs = 1.0e9                                  # example: 1 GS/s
n = np.arange(64)                           # sample numbers


def alias(f, fs):
    """Frequency (0 ... fs/2) at which a tone at f appears after sampling."""
    f = f % fs
    return min(f, fs - f)


ref = np.cos(2 * np.pi * 0.1e9 * n / fs)    # 100 MHz tone
for f in (0.1e9, 0.9e9, 1.1e9, 1.9e9):      # zones 1, 2, 3, 4
    x = np.cos(2 * np.pi * f * n / fs)
    print("%4.1f GHz -> %3.0f MHz  same samples: %s"
          % (f / 1e9, alias(f, fs) / 1e6, np.allclose(x, ref)))
