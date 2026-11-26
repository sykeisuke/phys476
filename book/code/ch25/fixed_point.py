"""NumPy model of the Vitis HLS type ap_fixed<W, I, Q, O> (Chapter 25).

W = total bits, I = integer bits INCLUDING the sign bit, F = W - I fraction bits.
Q (quantization): 'TRN' (default, toward -inf), 'RND' (round half up),
                  'RND_CONV' (round half to even).
O (overflow):     'WRAP' (default, keep the low W bits), 'SAT' (clip to the range).
Run:  python3 fixed_point.py      prints PASS
"""
import numpy as np


def fx_range(W, I, signed=True):
    F = W - I
    lsb = 2.0 ** -F
    if signed:
        return -2.0 ** (I - 1), 2.0 ** (I - 1) - lsb, lsb
    return 0.0, 2.0 ** I - lsb, lsb


def quantize(x, W, I, Q="TRN", O="WRAP", signed=True):
    x = np.asarray(x, dtype=float)
    F = W - I
    s = x * 2.0 ** F                         # value in units of the LSB
    if Q == "TRN":
        n = np.floor(s)
    elif Q == "RND":
        n = np.floor(s + 0.5)
    elif Q == "RND_CONV":
        n = np.round(s)                      # NumPy rounds half to even
    else:
        raise ValueError(Q)
    lo = -2 ** (W - 1) if signed else 0
    hi = 2 ** (W - 1) - 1 if signed else 2 ** W - 1
    if O == "SAT":
        n = np.clip(n, lo, hi)
    elif O == "WRAP":
        n = np.mod(n - lo, 2 ** W) + lo      # keep the low W bits
    else:
        raise ValueError(O)
    return n * 2.0 ** -F


def to_bits(x, W, I, Q="TRN", O="WRAP"):
    """The W-bit pattern (as a string) that stores x in ap_fixed<W,I>."""
    n = int(round(quantize(x, W, I, Q, O) * 2.0 ** (W - I)))
    return format(n & (2 ** W - 1), f"0{W}b")


def int_bits(max_abs):
    """Smallest I (with sign bit) whose range holds +-max_abs."""
    return int(np.floor(np.log2(max_abs))) + 2


if __name__ == "__main__":
    lo, hi, lsb = fx_range(16, 6)
    print(f"ap_fixed<16,6>: {lo} ... {hi}, LSB {lsb}")
    x = np.array([3.0, 31.9, 40.0, 50.0])
    print("WRAP:", quantize(x, 16, 6), " SAT:", quantize(x, 16, 6, O="SAT"))
    print("int bits for 50:", int_bits(50), " for 31:", int_bits(31), " for 32:", int_bits(32))
    ok = (lo, hi, lsb) == (-32.0, 32 - 2 ** -10, 2 ** -10)
    ok &= quantize(50.0, 16, 6) == -14.0                     # 50 wraps to 50 - 64
    ok &= quantize(50.0, 16, 6, O="SAT") == hi
    ok &= quantize(50.0, 16, 7) == 50.0
    ok &= quantize(-0.3, 8, 6) == -0.5                       # TRN goes toward -inf
    ok &= quantize(0.375, 8, 6, Q="RND") == 0.5              # half rounds up
    ok &= quantize(0.375, 8, 6, Q="RND_CONV") == 0.5         # 1.5 LSB -> 2 (even)
    ok &= quantize(0.625, 8, 6, Q="RND_CONV") == 0.5         # 2.5 LSB -> 2 (even)
    ok &= (int_bits(50), int_bits(31), int_bits(32)) == (7, 6, 7)
    ok &= quantize(3.0, 16, 6) * 2 ** 10 == 0x0C00           # 3.0 as raw bits
    ok &= to_bits(3.25, 8, 4) == "00110100"                   # 3.25 * 16 = 52
    ok &= to_bits(-1.5, 8, 4) == "11101000"                   # -1.5 * 16 = -24
    ok &= quantize(-0.3, 6, 4) == -0.5 and quantize(-0.3, 6, 4, Q="RND") == -0.25
    print("3.25 in <8,4>:", to_bits(3.25, 8, 4), "  -1.5 in <8,4>:", to_bits(-1.5, 8, 4))
    print("PASS" if ok else "FAIL")
