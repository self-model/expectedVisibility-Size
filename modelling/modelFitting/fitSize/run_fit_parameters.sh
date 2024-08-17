#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --job-name=fit_parameters_size_intersect
#SBATCH --partition=short
#SBATCH --time=10:00:00
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=lady7504@ox.ac.uk

module purge
module load MATLAB/R2023a
matlab -nodisplay -nosplash < fit_parameters_size_intersect.m > run_intersect.log
continue

