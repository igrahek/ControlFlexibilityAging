#!/bin/bash

#SBATCH --account=carney-ashenhav-condo
#SBATCH --time=800:00:00
#SBATCH --mem=128G
#SBATCH -n 4
#SBATCH -N 1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ivan_grahek@brown.edu
#SBATCH -J CAC_Aging_AllSubs_M1_LinearAge
#SBATCH -o R-%x.%j.out
module purge
module load anaconda
source activate pyHSSM_New_Nov24
python ../models/Model1.py