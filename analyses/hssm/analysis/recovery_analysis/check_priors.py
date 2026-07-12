"""
Prior-fix verification helper.

(1) Prints what the OLD vs proposed NEW z/a intercept priors imply, and
(2) if given a fitted model, extracts the key parameters so you can compare an
    old (biased-prior) fit to a new (corrected-prior) fit:
      - z_Intercept  should move from ~0.40 toward ~0.50 (unbiased) after the fix
      - v_/a_ intercepts and the block×age / switch×age / SinceSwitch×age interactions
        should be UNCHANGED (that is the prior-robustness check for the paper's results)

Usage:
    # priors only:
    python check_priors.py
    # + a fitted InferenceData (opens only the posterior group):
    python check_priors.py /path/to/output/Model1
"""
import sys
import numpy as np

def prior_report():
    try:
        from scipy import stats
    except Exception:
        print("(scipy not available; skipping prior distribution report)"); return
    print("== z Intercept prior (z in [0,1]; 0.5 = unbiased) ==")
    w = stats.weibull_min(c=1.5, scale=0.3)   # OLD: Weibull(alpha=1.5, beta=0.3)
    print(f"  OLD Weibull(1.5,0.3): mean={w.mean():.3f}  P(z<0.5)={w.cdf(0.5):.2f}  -> BIASED to error bound")
    b = stats.beta(5, 5)                       # NEW: Beta(5,5)
    print(f"  NEW Beta(5,5):        mean={b.mean():.3f}  P(z<0.5)={b.cdf(0.5):.2f}  sd={b.std():.3f}  -> centered/unbiased")
    print("== a Intercept prior (threshold > 0; fitted ~1.07) ==")
    s0, r0 = 0.5**2/1.75**2, 0.5/1.75**2
    g0 = stats.gamma(a=s0, scale=1/r0)         # OLD: Gamma(mu=0.5, sigma=1.75)
    print(f"  OLD Gamma(mu=.5,sd=1.75): shape={s0:.3f} median={g0.median():.3f} -> spike at 0, over-diffuse")
    s1, r1 = 1.5**2/1.0**2, 1.5/1.0**2
    g1 = stats.gamma(a=s1, scale=1/r1)         # NEW: Gamma(mu=1.5, sigma=1.0)
    print(f"  NEW Gamma(mu=1.5,sd=1.0): shape={s1:.2f} mode={(s1-1)/r1:.2f} mean={g1.mean():.2f} -> sensible around ~1")

def posterior_report(path):
    import xarray as xr
    ds = xr.open_dataset(path, group="posterior", engine="h5netcdf")
    print(f"\n== posterior params from {path} ==")
    want = ["z_Intercept", "z_congruency", "t_Intercept", "a_Intercept", "v_Intercept", "sv",
            # substantive interactions (names differ by model; only present ones print):
            "v_intervalType:blockType:Age", "a_intervalType:blockType:Age",
            "v_intervalType:Switch:Age", "a_intervalType:Switch:Age",
            "v_intervalType:SinceSwitch:Age", "a_intervalType:SinceSwitch:Age"]
    for v in want:
        if v in ds:
            x = ds[v].values.reshape(-1)
            print(f"  {v:34s} {x.mean():+.3f}  [{np.percentile(x,2.5):+.3f}, {np.percentile(x,97.5):+.3f}]")

if __name__ == "__main__":
    prior_report()
    if len(sys.argv) > 1:
        posterior_report(sys.argv[1])
