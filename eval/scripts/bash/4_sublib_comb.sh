#!/bin/bash
#SBATCH --job-name=split-pipe
#SBATCH --time=04-00
#SBATCH --ntasks=48
#SBATCH --mem=128G
#SBATCH --partition long
#SBATCH --output=/vast/scratch/users/moore.z/pdo_compar/eval/scripts/bash/4_sublib_comb.out

#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=moore.z@wehi.edu.au

conda init bash
conda activate spipe
cd /vast/scratch/users/moore.z/pdo_compar

split-pipe \
--mode comb \
--output_dir data/processed/SCRNA_240430_240621/ \
--sublibraries data/processed/SCRNA_240430/ data/processed/SCRNA_240621/
