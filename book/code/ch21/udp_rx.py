"""Receive UDP packets on a PC and print them.

Run:  python3 udp_rx.py [port]      (Ctrl-C to stop)
"""
import socket
import sys

port = int(sys.argv[1]) if len(sys.argv) > 1 else 5001
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind(("0.0.0.0", port))               # all interfaces
print("listening on UDP port", port, flush=True)
while True:
    data, addr = sock.recvfrom(2048)
    print(addr, data, flush=True)
