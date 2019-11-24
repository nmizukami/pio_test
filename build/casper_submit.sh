#!/bin/bash -l
#SBATCH --job-name=pio_job
#SBATCH --account=P48500028
#SBATCH --ntasks=5
#SBATCH --cpus-per-task=1
#SBATCH --time=00:10:00
#SBATCH --partition=dav
#SBATCH --output=pio_job.out.%j
#SBATCH --error=pio_job.err.%j

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

module load peak_memusage

### Run program
srun peak_memusage.exe ../bin/PIOtest.exe
