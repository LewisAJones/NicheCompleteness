#!/bin/bash
#SBATCH -t 25:00:00
#SBATCH -p thinnodes
#SBATCH -n 24
#SBATCH --job-name NicheCompleteness
#SBATCH --mail-type=begin 
#SBATCH --mail-type=end 
#SBATCH --mail-user=LewisAlan.Jones@uvigo.es
module load cesga/2020
Rscript virtual-species-random.R