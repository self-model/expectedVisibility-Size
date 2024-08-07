#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --job-name=fit_parameters
#SBATCH --partition=short
#SBATCH --time=8:00:00
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=alls0672@ox.ac.uk

module purge
module load MATLAB/R2023a
matlab -nodisplay -nosplash < fit_parameters.m > run.log
continue

