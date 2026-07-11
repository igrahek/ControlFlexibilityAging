import os, numpy as np, xarray as xr
ds=xr.open_dataset(os.path.expanduser("~/data/igrahek/aging_switch-repeat/analyses/hssm/output/Model4"), group="posterior", engine="h5netcdf")
def d(v): return ds[v].values.reshape(-1)
# model Age = 0.01216*years - 0.6337  (same scaling as Model3)
m,b=0.01216021677267226,-0.633735981409166
Y,O=m*25+b, m*75+b   # young=25, old=75
def summ(x):
    lo,hi=np.percentile(x,[2.5,97.5]); return x.mean(),lo,hi
print("Recovery slope = d(Speed<->Accuracy config gap)/d(intervals-since-switch).")
print("Positive = gap rebuilds across intervals (undershoot recovers). Steeper = faster transition.")
print("Prediction (slower dynamics with age): OLD slope < YOUNG slope.\n")
for par,lab in [("v","DRIFT"),("a","BOUNDARY")]:
    IS=d(par+"_intervalType:SinceSwitch"); ISA=d(par+"_intervalType:SinceSwitch:Age")
    sy=IS+ISA*Y; so=IS+ISA*O; diff=so-sy
    my,lyo,hyo=summ(sy); mo,lo,ho=summ(so); md,ld,hd=summ(diff)
    print("%s:"%lab)
    print("  young(25) recovery slope = %+.4f  [%+.4f,%+.4f]  P(slope>0)=%.3f"%(my,lyo,hyo,(sy>0).mean()))
    print("  old(75)   recovery slope = %+.4f  [%+.4f,%+.4f]  P(slope>0)=%.3f"%(mo,lo,ho,(so>0).mean()))
    print("  OLD - YOUNG              = %+.4f  [%+.4f,%+.4f]  P(old<young / SLOWER with age)=%.3f"%(md,ld,hd,(diff<0).mean()))
    print()
