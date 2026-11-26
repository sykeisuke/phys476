"""Fit the muon lifetime to the times measured by the coarse TDC testbench.

Run after the testbench (it writes muon_tdc_ns.txt: true_ns measured_ns per line):
    python fit_lifetime.py [muon_tdc_ns.txt]
"""
import sys

import numpy as np

fname = sys.argv[1] if len(sys.argv) > 1 else "muon_tdc_ns.txt"
true_ns, meas_ns = np.loadtxt(fname, unpack=True)
t = meas_ns / 1000.0                         # microseconds
tmin, tmax = 0.1, 10.0                       # fit range in us
sel = t[(t >= tmin) & (t < tmax)]

# maximum likelihood for an exponential truncated to [tmin, tmax): scan tau
taus = np.linspace(1.5, 3.0, 3001)
ll = [-len(sel) * np.log(tau * (np.exp(-tmin / tau) - np.exp(-tmax / tau)))
      - sel.sum() / tau for tau in taus]
tau = taus[int(np.argmax(ll))]
res = meas_ns - true_ns                      # TDC error per event
print(f"events in fit : {len(sel)}")
print(f"tau           : {tau:.3f} us  (+- {tau / np.sqrt(len(sel)):.3f} us statistical)")
print(f"TDC error     : mean {res.mean():+.1f} ns, RMS {res.std():.1f} ns, "
      f"range {res.min():+.0f} .. {res.max():+.0f} ns")
