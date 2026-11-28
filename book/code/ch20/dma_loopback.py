"""PS -> PL -> PS loopback through an AXI DMA, with the PYNQ library.

Runs on the PYNQ-Z2 (in a Jupyter notebook or with python3 on the board),
not on a PC. dma_loop.bit and dma_loop.hwh (same name) must be in the same
folder; the DMA instance in the block design is called axi_dma_0.
"""
import time

import numpy as np
from pynq import Overlay, allocate

ol = Overlay("dma_loop.bit")             # programs the PL
dma = ol.axi_dma_0

n = 1024
src = allocate(shape=(n,), dtype=np.uint32)   # physically contiguous buffers
dst = allocate(shape=(n,), dtype=np.uint32)
src[:] = np.arange(n) + 1000

# MM2S sends the n words as one packet, with tlast on the last word;
# axis_pedsub passes tlast through, and S2MM ends its transfer there.
dma.recvchannel.transfer(dst)            # S2MM: PL -> DDR
dma.sendchannel.transfer(src)            # MM2S: DDR -> PL
dma.sendchannel.wait()
dma.recvchannel.wait()
print("loopback OK:", np.array_equal(dst, src - 100))   # axis_pedsub, PED = 100

# throughput: one large transfer (the DMA's buffer-length register must be wide enough)
big = 256 * 1024                         # 256 k words = 1 MiB
bsrc = allocate(shape=(big,), dtype=np.uint32)
bdst = allocate(shape=(big,), dtype=np.uint32)
bsrc[:] = np.arange(big, dtype=np.uint32)
t0 = time.perf_counter()
dma.recvchannel.transfer(bdst)
dma.sendchannel.transfer(bsrc)
dma.sendchannel.wait()
dma.recvchannel.wait()
dt = time.perf_counter() - t0
print("1 MiB in %.2f ms -> %.0f MB/s" % (dt * 1e3, 4 * big / dt / 1e6))

for b in (src, dst, bsrc, bdst):
    b.freebuffer()
