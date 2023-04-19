#!/bin/bash

nshell_values=(5 10)
nfock_values=(1 3 11)

for nshell in "${nshell_values[@]}"; do
  for nfock in "${nfock_values[@]}"; do
    sbatch "file_${nshell}_${nfock}.inp"
  done
done


