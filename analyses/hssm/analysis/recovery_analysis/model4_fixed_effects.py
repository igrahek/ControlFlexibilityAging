import os, numpy as np, xarray as xr
try:
    import arviz as az; AZv=az.__version__
except Exception as e:
    az=None; AZv=str(e)
path=os.path.expanduser("~/data/igrahek/aging_switch-repeat/analyses/hssm/output/Model4")
ds=None
for eng in ["h5netcdf","netcdf4"]:
    try: ds=xr.open_dataset(path, group="posterior", engine=eng); break
    except Exception as e: last=e
if ds is None: raise last
print("arviz:",AZv,"| chains",ds.sizes["chain"],"draws",ds.sizes["draw"])
pop=[v for v in ds.data_vars if set(ds[v].dims)<= {"chain","draw"}]
def rr(vals):
    rh=es=float("nan")
    if az is not None:
        try: rh=float(az.rhat(xr.DataArray(vals,dims=["chain","draw"])))
        except Exception: pass
        try: es=float(az.ess(xr.DataArray(vals,dims=["chain","draw"])))
        except Exception: pass
    return rh,es
order=["v_Intercept","v_intervalType","v_SinceSwitch","v_Age","v_congruency","v_intervalType:SinceSwitch","v_intervalType:Age","v_SinceSwitch:Age","v_intervalType:SinceSwitch:Age","a_Intercept","a_intervalType","a_SinceSwitch","a_Age","a_scaledRunningTime","a_intervalType:SinceSwitch","a_intervalType:Age","a_SinceSwitch:Age","a_intervalType:SinceSwitch:Age"]
missing=[v for v in order if v not in ds]
if missing:
    print("MISSING:",missing); print("available:",sorted(pop))
rows=[]
for v in order:
    if v not in ds: continue
    vals=ds[v].values; x=vals.reshape(-1)
    lo,hi=np.percentile(x,[2.5,97.5]); p=(x<0).mean(); tail=min(p,1-p)
    rh,es=rr(vals)
    st="***" if tail<0.001 else "**" if tail<0.01 else "*" if tail<0.05 else "n.s."
    rows.append((v,x.mean(),x.std(ddof=1),lo,hi,p,max(p,1-p),tail,st,rh,es))
def f(r): return "%-32s %8.3f %6.3f [%7.3f,%7.3f] P(<0)=%5.3f pd=%5.3f p=%6.4f %-4s Rhat=%5.3f ESS=%6.0f"%r
print("="*150); print("DRIFT (v) | intervalType Acc=+.5/Speed=-.5 ; SinceSwitch 0=switch..4 ; Age scaled ; congruency cong=+.5"); print("-"*150)
for r in rows:
    if r[0][0]=="v": print(f(r))
print(); print("BOUNDARY (a)"); print("-"*150)
for r in rows:
    if r[0][0]=="a": print(f(r))
print("="*150)
rh=[r[9] for r in rows if r[9]==r[9]]; es=[r[10] for r in rows if r[10]==r[10]]
print("max Rhat=%.4f | min ESS=%.0f  (population-level fixed effects)"%(max(rh),min(es)))
