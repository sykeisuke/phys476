"""Number conversions for hardware (Chapter 0).

Run:  python3 base_convert.py            (prints the worked examples)
      python base_convert.py 0xA5 173   (converts your own values)

The functions do the conversion by hand (repeated division by 2, groups of four
bits) so you can compare each step with your paper calculation. Python's own
bin(), hex() and int(text, base) are used only to check the result.
"""
import math
import sys


def to_binary(n, width=None):
    """Decimal -> binary string by repeated division by 2 (remainders, read upward)."""
    if n == 0:
        bits = "0"
    else:
        rem = []
        while n > 0:
            rem.append(str(n % 2))   # remainder = next bit, least significant first
            n //= 2
        bits = "".join(reversed(rem))
    if width:
        bits = bits.zfill(width)
    return bits


def to_hex(n):
    """Decimal -> hexadecimal: group the binary digits in fours from the right."""
    bits = to_binary(n)
    bits = bits.zfill(-(-len(bits) // 4) * 4)          # pad to a multiple of 4
    digits = "0123456789ABCDEF"
    return "".join(digits[int(bits[i:i + 4], 2)] for i in range(0, len(bits), 4))


def from_base(text):
    """'0xA5', '0b10100101' or '165' -> integer, by summing digit x place value."""
    text = text.strip().lower().replace("_", "")
    base, body = 10, text
    if text.startswith("0x"):
        base, body = 16, text[2:]
    elif text.startswith("0b"):
        base, body = 2, text[2:]
    value = 0
    for ch in body:
        value = value * base + int(ch, 16)   # shift one place left, add the digit
    return value


def bits_needed(n):
    """Smallest number of bits that can hold the unsigned values 0 .. n."""
    return max(1, math.ceil(math.log2(n + 1)))


def show(n):
    w = bits_needed(n)
    b = to_binary(n, -(-w // 4) * 4)
    nibbles = " ".join(b[i:i + 4] for i in range(0, len(b), 4))
    print(f"{n:>8d} = 0b{nibbles} = 0x{to_hex(n)}   ({w} bits needed)")
    assert int(b, 2) == n and int(to_hex(n), 16) == n   # check against Python
    assert bin(n)[2:] == to_binary(n)


if __name__ == "__main__":
    if len(sys.argv) > 1:
        for arg in sys.argv[1:]:
            show(from_base(arg))
        sys.exit()

    print("Worked examples")
    for text in ("0xA5", "173", "0b1101", "255", "256", "1000", "0xFF", "4095"):
        print(f"{text:>8s} ->", end="")
        show(from_base(text))

    print("\nUnsigned range of an N-bit number: 0 .. 2^N - 1")
    for n in (1, 4, 8, 10, 12, 16, 32):
        print(f"  N = {n:2d}: 0 .. {2**n - 1:,}")

    print("\nPowers of two: 2^10 is about 10^3")
    for k in (10, 20, 30):
        print(f"  2^{k} = {2**k:>13,}   (about 10^{k * 3 // 10})")

    print("\nHow many bits for ...")
    for what, n in (("a 12-bit ADC sample (max 4095)", 4095),
                    ("1 s of 10 ns clock cycles (10^8)", 10**8),
                    ("a 64-channel number (0..63)", 63)):
        print(f"  {what}: {bits_needed(n)} bits")
