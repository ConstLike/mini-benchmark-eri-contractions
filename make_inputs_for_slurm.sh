#!/bin/bash

nshell_values=(5 10)
nfock_values=(1 3 11)

for nshell in "${nshell_values[@]}"; do
  for nfock in "${nfock_values[@]}"; do
     file_out="file_${nshell}_${nfock}.inp"
     echo "#!/bin/bash" >> "$file_out"
     echo "#SBATCH --job-name=runner" >> "$file_out"
     echo "#SBATCH --ntasks=1" >> "$file_out"
     echo "#SBATCH --cpus-per-task=1" >> "$file_out"
#    echo "#SBATCH --partition r630" >> "$file_out"
     echo "#SBATCH --output=file_${nshell}_${nfock}.log" >> "$file_out"
     echo "echo \"basis,nfock,FI,IF\"" >> "$file_out"
     echo "output=\$(./runner $nshell $nfock)" >> "$file_out"
     echo "num_basis=\$(echo \"\$output\" | awk '/Number of basis functions/ {print \$NF}')" >> "$file_out"
     echo "fi_time=\$(echo \"\$output\" | awk '/FI elapsed time/ {print \$NF}')" >> "$file_out"
     echo "if_time=\$(echo \"\$output\" | awk '/IF elapsed time/ {print \$NF}')" >> "$file_out"
     echo "printf \"%s\n\" \"\$num_basis,$nfock,\$fi_time,\$if_time\"" >> "$file_out"
  done
done
