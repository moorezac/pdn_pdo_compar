#!/bin/bash
#SBATCH --job-name=split-pipe
#SBATCH --time=04-00
#SBATCH --ntasks=48
#SBATCH --mem=128G
#SBATCH --partition long
#SBATCH --output=/vast/scratch/users/moore.z/pdo_compar/eval/scripts/bash/3_sublib_1.out

#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=moore.z@wehi.edu.au

# conda init
conda activate spipe
cd /vast/scratch/users/moore.z/pdo_compar

split-pipe \
--mode all \
--chemistry v2 \
--genome_dir meta/genome/hg38/ \
--fq1 data/raw/SCRNA_240430/X201SC24040846-Z01-F001/01.RawData/Library_01/sub_1_cat_r1.fq.gz \
--fq2 data/raw/SCRNA_240430/X201SC24040846-Z01-F001/01.RawData/Library_01/sub_1_cat_r2.fq.gz \
--output_dir data/processed/SCRNA_240430 \
--samp_sltab meta/sample_table.xlsm
