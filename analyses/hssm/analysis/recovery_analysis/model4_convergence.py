import os, numpy as np, xarray as xr, arviz as az
path=os.path.expanduser("~/data/igrahek/aging_switch-repeat/analyses/hssm/output/Model4")
ds=xr.open_dataset(path, group="posterior", engine="h5netcdf")
order=["v_Intercept","v_intervalType","v_SinceSwitch","v_Age","v_congruency","v_intervalType:SinceSwitch","v_intervalType:Age","v_SinceSwitch:Age","v_intervalType:SinceSwitch:Age","a_Intercept","a_intervalType","a_SinceSwitch","a_Age","a_scaledRunningTime","a_intervalType:SinceSwitch","a_intervalType:Age","a_SinceSwitch:Age","a_intervalType:SinceSwitch:Age"]
order=[v for v in order if v in ds]
idata=az.InferenceData(posterior=ds[order])
s=az.summary(idata, var_names=order, hdi_prob=0.95, kind="diagnostics")
cols=[c for c in ["ess_bulk","ess_tail","r_hat"] if c in s.columns]
print("convergence (population-level fixed effects):")
print("%-34s %8s %8s %6s"%("param","ess_bulk","ess_tail","r_hat"))
for v in order:
    print("%-34s %8.0f %8.0f %6.3f"%(v, s.loc[v,"ess_bulk"], s.loc[v,"ess_tail"], s.loc[v,"r_hat"]))
print("-"*60)
print("max r_hat = %.4f | min ess_bulk = %.0f | min ess_tail = %.0f"%(s.r_hat.max(), s.ess_bulk.min(), s.ess_tail.min()))
# also whole-model diagnostics across ALL params (incl random effects) for a global convergence check
sall=az.summary(az.InferenceData(posterior=ds), hdi_prob=0.95, kind="diagnostics")
print("WHOLE MODEL (incl. all random effects): max r_hat=%.3f | %% params r_hat>1.01: %.1f%% | min ess_bulk=%.0f"%(sall.r_hat.max(), 100*(sall.r_hat>1.01).mean(), sall.ess_bulk.min()))
