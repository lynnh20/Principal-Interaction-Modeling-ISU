#!/bin/bash

#SBATCH --time=08:00:00   # walltime limit (HH:MM:SS)
#SBATCH --nodes=1   # number of nodes
#SBATCH --ntasks-per-node=16   # 16 processor core(s) per node 
#SBATCH --error=job.%J.err 
#SBATCH --output=job.%J.out 
#SBATCH --mail-user=fangshu.stat@gmail.com   # email address
#SBATCH --mail-type=END
module load r 
Rscript Two_Stage_Main.R --i $1 > Two_Stage_$SLURM_JOBID.Rout

