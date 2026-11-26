"""Build an Ethernet/IPv4/UDP frame carrying "Hello World" by hand.

This is what a hand-built FPGA sender must produce byte by byte.
Run:  python3 build_udp_frame.py
"""
import struct
import zlib

SRC_MAC = bytes.fromhex("020000000001")   # FPGA: locally administered address
DST_MAC = bytes.fromhex("020000000064")   # PC: replace with its real MAC (from ARP)
SRC_IP = bytes([192, 168, 1, 50])          # FPGA
DST_IP = bytes([192, 168, 1, 100])         # PC
SRC_PORT, DST_PORT = 5000, 5001
payload = b"Hello World"


def ip_checksum(hdr):
    s = sum(struct.unpack("!10H", hdr))
    while s >> 16:                         # fold the carries back in
        s = (s & 0xFFFF) + (s >> 16)
    return ~s & 0xFFFF


udp = struct.pack("!HHHH", SRC_PORT, DST_PORT, 8 + len(payload), 0)  # checksum 0 = unused
ip = struct.pack("!BBHHHBBH4s4s",
                 0x45, 0x00,               # version 4, IHL 5 (20 bytes); DSCP/ECN
                 20 + len(udp) + len(payload),
                 0x0000, 0x4000,           # identification; flags: don't fragment
                 64, 17, 0,                # TTL, protocol 17 = UDP, checksum 0 for now
                 SRC_IP, DST_IP)
ip = ip[:10] + struct.pack("!H", ip_checksum(ip)) + ip[12:]

frame = DST_MAC + SRC_MAC + struct.pack("!H", 0x0800) + ip + udp + payload
frame += bytes(max(0, 60 - len(frame)))   # pad to 60 bytes (64 with the FCS)
fcs = struct.pack("<I", zlib.crc32(frame))  # CRC-32, sent least significant byte first
frame += fcs

print("IP header :", ip.hex(" "))
print("checksum  : 0x%04x  (check of the full header gives %d)" %
      (ip_checksum(ip[:10] + b"\0\0" + ip[12:]), ip_checksum(ip)))
print("frame     : %d bytes including the FCS" % len(frame))
print(frame.hex(" "))
assert ip_checksum(ip) == 0
assert zlib.crc32(frame) == 0x2144DF1C    # CRC over frame + FCS: fixed residue
print("OK")
