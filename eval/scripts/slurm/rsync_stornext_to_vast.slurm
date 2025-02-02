#!/bin/bash
#SBATCH --job-name=rsync_vast_to_stornext
#SBATCH --time=01-00
#SBATCH --ntasks=16
#SBATCH --mem=64G
#SBATCH --partition datamover
#SBATCH --output=/vast/scratch/users/moore.z/pdo_compar/eval/scripts/slurm/vast_to_stornext.txt

#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=moore.z@wehi.edu.au

# rsync --verbose --progress --compress --recursive --times --perms  \
#   --exclude data/raw \
#   --exclude data/processed \ 
#   /stornext/Bioinf/data/lab_brain_cancer/projects/pdo_compar/ \ 
#   /vast/scratch/users/moore.z/pdo_compar/

rsync --verbose --progress --compress --recursive --times --perms /vast/scratch/users/moore.z/pdo_compar/eval/ /stornext/Bioinf/data/lab_brain_cancer/projects/pdo_compar/eval/
rsync --verbose --progress --compress --recursive --times --perms /vast/scratch/users/moore.z/pdo_compar/meta/ /stornext/Bioinf/data/lab_brain_cancer/projects/pdo_compar/meta/

rsync --verbose --progress --compress --recursive --times --perms /stornext/Bioinf/data/lab_brain_cancer/data_processed/SCRNA/SCRNA_240430/ /vast/scratch/users/moore.z/pdo_compar/data/processed/SCRNA_240430/
rsync --verbose --progress --compress --recursive --times --perms /stornext/Bioinf/data/lab_brain_cancer/data_processed/SCRNA/SCRNA_240621/ /vast/scratch/users/moore.z/pdo_compar/data/processed/SCRNA_240621/
rsync --verbose --progress --compress --recursive --times --perms /stornext/Bioinf/data/lab_brain_cancer/data_processed/SCRNA/SCRNA_240430_240621/ /vast/scratch/users/moore.z/pdo_compar/data/processed/SCRNA_240430_240621/

rsync --verbose --progress --compress --recursive --times --perms /stornext/Bioinf/data/lab_brain_cancer/data_raw/SCRNA/SCRNA_240430/ /vast/scratch/users/moore.z/pdo_compar/data/raw/SCRNA_240430/ 
rsync --verbose --progress --compress --recursive --times --perms /stornext/Bioinf/data/lab_brain_cancer/data_raw/SCRNA/SCRNA_240621/ /vast/scratch/users/moore.z/pdo_compar/data/raw/SCRNA_240621/ 
rsync --verbose --progress --compress --recursive --times --perms /stornext/Bioinf/data/lab_brain_cancer/data_raw/MTH_240801/ /vast/scratch/users/moore.z/pdo_compar/data/raw/MTH_240801/
