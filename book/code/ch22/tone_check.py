"""Lab B analysis: find the frequency of a captured DAC tone and compare it
with the alias you predicted (Chapter 22). Runs on a PC or on the board.

Real samples (mixer bypassed):
    python tone_check.py capture.npy --fs 4e9 --tone 1.1e9
I/Q samples (receiver mixes by f_nco, then decimates to fs_out):
    python tone_check.py capture.npy --fs-adc 4e9 --fs 500e6 --nco 1e9 --tone 1.01e9
Without a file it simulates a capture first (no board needed):
    python tone_check.py --demo
Use the sampling rates, NCO and decimation that your notebook reports;
the numbers above are only examples, not board settings.
"""
import argparse

import numpy as np


def alias(f, fs):
    """Frequency (0 ... fs/2) at which a real tone at f appears after sampling."""
    f = f % fs
    return min(f, fs - f)


def fold(f, fs):
    """Fold a frequency into -fs/2 ... +fs/2 (complex sampling)."""
    return (f + fs / 2) % fs - fs / 2


def predict(tone, fs, fs_adc=None, nco=None):
    if nco is None:                       # real samples at fs
        return alias(tone, fs)
    a = alias(tone, fs_adc)               # the ADC sees +a and -a
    cands = [fold(+a - nco, fs), fold(-a - nco, fs)]
    return min(cands, key=abs)            # the decimation filter keeps the band near 0


def peak(x, fs):
    """Frequency of the strongest FFT bin, refined by a parabola (Hz)."""
    x = np.asarray(x)
    x = x - x.mean()
    spec = np.abs(np.fft.fft(x * np.hanning(x.size)))
    f = np.fft.fftfreq(x.size, 1 / fs)
    if not np.iscomplexobj(x):            # real data: positive half only
        spec[f < 0] = 0
    k = int(np.argmax(spec))
    a, b, c = np.log(spec[k - 1] + 1e-12), np.log(spec[k] + 1e-12), np.log(spec[(k + 1) % x.size] + 1e-12)
    d = 0.5 * (a - c) / (a - 2 * b + c)   # sub-bin offset
    return f[k] + d * fs / x.size


def simulate(tone, fs, fs_adc=None, nco=None, n=8192, seed=1):
    rng = np.random.default_rng(seed)
    if nco is None:
        t = np.arange(n) / fs
        return 1000 * np.cos(2 * np.pi * tone * t) + rng.normal(0, 5, n)
    D = int(round(fs_adc / fs))
    t = np.arange(n * D) / fs_adc
    x = np.cos(2 * np.pi * tone * t) + rng.normal(0, 0.005, t.size)
    iq = x * np.exp(-2j * np.pi * nco * t)
    taps = np.arange(-64, 65)
    cut = 0.4 * fs / fs_adc
    h = 2 * cut * np.sinc(2 * cut * taps) * np.hamming(taps.size)
    return np.convolve(iq, h, mode="same")[::D]


def report(x, tone, fs, fs_adc=None, nco=None):
    fm, fp = peak(x, fs), predict(tone, fs, fs_adc, nco)
    ok = abs(fm - fp) < 3 * fs / len(x)
    print(f"tone {tone / 1e6:9.2f} MHz: measured {fm / 1e6:9.2f} MHz, "
          f"predicted {fp / 1e6:9.2f} MHz  {'OK' if ok else 'MISMATCH'}")
    return ok


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("file", nargs="?")
    ap.add_argument("--fs", type=float, help="sample rate of the data in the file (Hz)")
    ap.add_argument("--fs-adc", type=float, help="ADC sample rate (I/Q mode)")
    ap.add_argument("--nco", type=float, help="receiver NCO frequency (I/Q mode)")
    ap.add_argument("--tone", type=float, help="DAC tone frequency (Hz)")
    ap.add_argument("--demo", action="store_true")
    a = ap.parse_args()
    if a.demo or a.file is None:
        print("demo, real samples at 1 GS/s (zones 1-4):")
        good = all(report(simulate(f, 1e9), f, 1e9) for f in (100e6, 900e6, 1.1e9, 1.9e9))
        print("demo, I/Q: ADC 4 GS/s, NCO 1 GHz, decimation 8 (500 MS/s):")
        good &= all(report(simulate(f, 500e6, 4e9, 1e9), f, 500e6, 4e9, 1e9)
                    for f in (1.01e9, 0.95e9, 3.05e9))
        print("PASS" if good else "FAIL")
    else:
        x = np.load(a.file)
        report(x, a.tone, a.fs, a.fs_adc, a.nco)
