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



def loglik(tau):
    """Log-likelihood of an exponential truncated to [tmin, tmax)."""
    return (-len(sel) * np.log(tau * (np.exp(-tmin / tau) - np.exp(-tmax / tau)))
            - sel.sum() / tau)


# maximum likelihood: scan tau
taus = np.linspace(1.5, 3.0, 3001)
tau = taus[int(np.argmax([loglik(x) for x in taus]))]
# statistical error from the curvature of the same (truncated) log-likelihood:
# sigma = 1 / sqrt(-d2 lnL / d tau2). The truncation makes it larger than tau/sqrt(N).
h = 0.01
curv = -(loglik(tau + h) - 2 * loglik(tau) + loglik(tau - h)) / h**2
sigma = 1.0 / np.sqrt(curv)
res = meas_ns - true_ns                      # TDC error per event
print(f"events in fit : {len(sel)}")
print(f"tau           : {tau:.3f} us  (+- {sigma:.3f} us statistical)")
print(f"TDC error     : mean {res.mean():+.1f} ns, RMS {res.std():.1f} ns, "
      f"range {res.min():+.0f} .. {res.max():+.0f} ns")
