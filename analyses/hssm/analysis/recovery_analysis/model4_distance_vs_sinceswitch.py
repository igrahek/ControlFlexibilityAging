import os, numpy as np, xarray as xr
ds=xr.open_dataset(os.path.expanduser("~/data/igrahek/aging_switch-repeat/analyses/hssm/output/Model4"), group="posterior", engine="h5netcdf")
def d(v): return ds[v].values.reshape(-1)
m,b=0.01216021677267226,-0.633735981409166
YOUNG,OLD=m*25+b, m*75+b
vI=d("v_intervalType"); vIS=d("v_intervalType:SinceSwitch"); vIA=d("v_intervalType:Age"); vISA=d("v_intervalType:SinceSwitch:Age")
aI=d("a_intervalType"); aIS=d("a_intervalType:SinceSwitch"); aIA=d("a_intervalType:Age"); aISA=d("a_intervalType:SinceSwitch:Age")
def Dv(SS,A): return vI+vIS*SS+vIA*A+vISA*SS*A      # Speed<->Accuracy gap in drift at (SinceSwitch,Age)
def Da(SS,A): return aI+aIS*SS+aIA*A+aISA*SS*A      # ... in threshold
allv=np.concatenate([Dv(s,a) for s in range(5) for a in (YOUNG,OLD)]); sv=allv.std()
alla=np.concatenate([Da(s,a) for s in range(5) for a in (YOUNG,OLD)]); sa=alla.std()
def DIST(SS,A): return np.sqrt((Dv(SS,A)/sv)**2+(Da(SS,A)/sa)**2)   # SD-scaled Euclidean distance
def q(x): return x.mean(),np.percentile(x,2.5),np.percentile(x,97.5)
print("Speed<->Accuracy distance in (v,a) space (SD-scaled) vs intervals-since-switch")
print("  transition => distance RISES with SS ; static in-between setpoint => FLAT\n")
for A,lab in [(YOUNG,"YOUNG(25)"),(OLD,"OLD(75)")]:
    print(" ",lab)
    for s in range(5):
        mn,lo,hi=q(DIST(s,A)); print("    SS=%d : %.3f  [%.3f, %.3f]"%(s,mn,lo,hi))
def rec(A): return DIST(4,A)-DIST(0,A)
ry,ro=rec(YOUNG),rec(OLD); diff=ro-ry
print("\nRecovery (distance at SS=4 minus SS=0):")
print("  young     : %+.3f [%+.3f,%+.3f]  P(recovery>0)=%.3f"%(*q(ry),(ry>0).mean()))
print("  old       : %+.3f [%+.3f,%+.3f]  P(recovery>0)=%.3f"%(*q(ro),(ro>0).mean()))
print("  OLD-YOUNG : %+.3f [%+.3f,%+.3f]  P(old<young / SLOWER with age)=%.3f"%(*q(diff),(diff<0).mean()))
r0=DIST(4,0.0)-DIST(0,0.0)
print("\nTransition test (whole sample, mean age): recovery = %+.3f [%+.3f,%+.3f]  P(>0 = genuine movement)=%.3f"%(*q(r0),(r0>0).mean()))
