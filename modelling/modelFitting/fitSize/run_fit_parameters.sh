#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --job-name=fit_parameters_size
#SBATCH --partition=short
#SBATCH --time=1:00:00
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=lady7504@ox.ac.uk

module purge
module load MATLAB/R2023a
matlab -nodisplay -nosplash < fit_parameters_size.m > run.log
continue

