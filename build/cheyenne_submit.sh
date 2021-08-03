#!/bin/bash
#PBS -A P48500028
#PBS -q premium 
#PBS -N pio_job 
#PBS -l walltime=0:10:00
#PBS -o pio_job.out
#PBS -e pio_job.err
#PBS -l select=6:ncpus=6:mpiprocs=6
#:mem=109G

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

module load peak_memusage
#module load intel
#module load openmpi

mpiexec_mpt peak_memusage.exe ../bin/pio_test.exe
#mpirun -n 32 peak_memusage.exe ../bin/pio_test.ext
