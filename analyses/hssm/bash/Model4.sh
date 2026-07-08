#!/bin/bash

#SBATCH --account=carney-frankmj-condo2
#SBATCH --partition=batch
#SBATCH --time=800:00:00
#SBATCH --mem=128G
#SBATCH -n 4
#SBATCH -N 1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ivan_grahek@brown.edu
#SBATCH -J CAC_Aging_AllSubs_M4_LinearAge_SinceSwitch
#SBATCH -o R-%x.%j.out
source /etc/profile.d/zz_activate_lmod_user.sh 2>/dev/null || true
$HOME/.conda/envs/pyHSSM_New_Nov24/bin/python ../models/Model4.py
