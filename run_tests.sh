#!/bin/bash

gfortran -o runner program.f90 -O3 -std=f2008 -Wall -Wextra -pedantic-errors

# Define the values for nshell and nfock
nshell_values=(10 15)
nfock_values=(1 5 7 11)

# Create an empty list to store the data
data=()

#echo "basis,nfock,FI,IF"

# Loop over the nshell and nfock values and run the program
for nshell in "${nshell_values[@]}"; do
  for nfock in "${nfock_values[@]}"; do
    ./runner $nshell $nfock
    output=$(./runner $nshell $nfock)
    num_basis=$(echo "$output" | awk '/Number of basis functions/ {print $NF}')
    fi_time=$(echo "$output" | awk '/FI elapsed time/ {print $NF}')
    if_time=$(echo "$output" | awk '/IF elapsed time/ {print $NF}')
    data+=("$num_basis,$nfock,$fi_time,$if_time")
#   printf "%s\n" "$num_basis,$nfock,$fi_time,$if_time"
  done
done

# Create a csv file
echo "basis,nfock,FI,IF" > data.csv
( IFS=$'\n'; echo "basis,nfock,FI,IF"; echo "${data[*]}" )\
  | awk -F' ' '{print $1,$2,$3,$4}'\
  | python3 -c 'import sys, csv; csv.writer(sys.stdout).writerows(csv.reader(sys.stdin))' > data.csv
