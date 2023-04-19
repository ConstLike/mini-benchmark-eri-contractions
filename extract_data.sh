#!/bin/bash

# Define the values for nshell and nfock
nshell_values=(5 10)
nfock_values=(1 3 11)

# Create the CSV file and add the header
echo "nshell,basis,nfock,FI,IF" > ext_data.csv

# Loop over the nshell and nfock values and extract the data
for nshell in "${nshell_values[@]}"; do
  for nfock in "${nfock_values[@]}"; do
    # Define the filename for this combination of nshell and nfock
    filename="file_${nshell}_${nfock}.log"

    # Extract the data from the file and append it to the CSV file
    basis=$(sed -n '2{p;q}' "$filename" | awk -F',' '{print $1}')
    fi=$(sed -n '2{p;q}' "$filename" | awk -F',' '{print $3}')
    if=$(sed -n '2{p;q}' "$filename" | awk -F',' '{print $4}')
      echo "$nshell,$basis,$nfock,$fi,$if" >> ext_data.csv
    done
done
