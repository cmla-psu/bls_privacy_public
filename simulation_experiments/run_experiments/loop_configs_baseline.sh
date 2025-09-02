#!/bin/bash

#SBATCH --job-name=apythonjob
#SBATCH --partition=open
#SBATCH --nodes=6
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --time=12:00:00

module load python/3.6 
module load anaconda3

for f in configs/baseline_reps/*.yaml; do python3 main.py $f; done
