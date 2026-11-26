"""Send numbered UDP packets of 32-bit data words (for example from the PYNQ-Z2).

On the board the data would come from a DMA buffer (Chapter 20); here a
counter stands in for it. Each packet starts with a 32-bit sequence number
so that the receiver can see lost packets.
Run:  python3 udp_send.py <pc-address> [port] [packets]
"""
import socket
import struct
import sys

import numpy as np

host = sys.argv[1] if len(sys.argv) > 1 else "127.0.0.1"
port = int(sys.argv[2]) if len(sys.argv) > 2 else 5001
npkt = int(sys.argv[3]) if len(sys.argv) > 3 else 3

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
for seq in range(npkt):
    words = np.arange(256, dtype=np.uint32) + 256 * seq     # 1 KB of data
    sock.sendto(struct.pack("!I", seq) + words.astype(">u4").tobytes(), (host, port))
print("sent", npkt, "packets of", 4 + 4 * 256, "bytes to", host, port)
