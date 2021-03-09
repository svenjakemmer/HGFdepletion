#!/bin/sh
########## Begin MOAB/Slurm header ##########
#
# Give job a reasonable name
#SBATCH -J D_1profiles1_1
#
# Request number of nodes and CPU cores per node for job
#SBATCH -n 16
#
# Estimated wallclock time for job
#SBATCH -t 110:00:00
#
# Request correct partition
#SBATCH --partition single
#
########### End MOAB header ##########

# Setup R Environment
module load math/R
export OPENBLAS_NUM_THREADS=1
# Start program
R CMD BATCH --no-save --no-restore --slave D_1profiles1_folder/D_1profiles1_1.R