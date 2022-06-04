#!/bin/bash

# Author: Francisco Javier Soriano Díaz

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  INPUT_DIR=$condition/Peaks

  OUT_FILE=$condition'_merged_peaks.bed'

  # List of name of peak files
  replicates=$(find . -type f -path '*'$INPUT_DIR'/*' -name '*.narrowPeak')

  # Merge the input peak file with HOMER
  mergePeaks -d given $replicates > tmp1.txt
  
  # Number or replicates
  num_rep=$(echo -n "$replicates" | grep -c '^')

  # Search condition: "|.*" for a merge of 2 peaks, "|.*|.*" for a merge of 3 peaks, etc.
  search_cond=$(printf "%0.s|.*" $(seq 2 $num_rep))

  # Select the lines containing merged peaks and transform them into bed format
  grep "$search_cond" tmp1.txt | cut -f 2-4 > ./$INPUT_DIR/$OUT_FILE

  # Remove temporary files
  rm tmp1.txt

done
