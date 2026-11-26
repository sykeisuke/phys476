"""Digital down-conversion in numpy: NCO mixer, low-pass filter, decimation.

Example numbers (not a board setting): fs = 1 GS/s, wanted tone 260 MHz,
unwanted tone 400 MHz, NCO 250 MHz, decimation D = 8.
Run:  python3 ddc_demo.py
"""
import numpy as np

fs, n = 1.0e9, 8192
t = np.arange(n) / fs
x = np.cos(2 * np.pi * 260e6 * t) + np.cos(2 * np.pi * 400e6 * t)   # real ADC samples

f_nco, D = 250e6, 8
iq = x * np.exp(-2j * np.pi * f_nco * t)    # mixer: 260 -> +10 MHz, 400 -> +150 MHz

taps = np.arange(-64, 65)                    # windowed-sinc low-pass, cutoff 40 MHz
cut = 40e6 / fs
h = 2 * cut * np.sinc(2 * cut * taps) * np.hamming(taps.size)


def spectrum_db(y, rate):
    spec = np.abs(np.fft.fft(y * np.hanning(y.size)))
    return np.fft.fftfreq(y.size, 1 / rate) / 1e6, 20 * np.log10(spec / spec.max() + 1e-12)


for label, y in (("no filter  ", iq[::D]),
                 ("with filter", np.convolve(iq, h, mode="same")[::D])):
    f, db = spectrum_db(y, fs / D)
    others = np.abs(f - 10) > 3                     # everything except the wanted +10 MHz
    k = np.argmax(np.where(others, db, -999))
    w = np.argmin(np.abs(f - 10))                  # bin of the wanted tone
    print(f"{label}: rate {fs / D / 1e6:.0f} MS/s; wanted +10 MHz; strongest other at "
          f"{f[k]:+.0f} MHz, {db[k] - db[w] + 0.0:.0f} dB relative to wanted")
