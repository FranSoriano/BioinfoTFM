#!/bin/bash

# Author: Francisco Javier Soriano Díaz

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'CreateNetwork_'$condition

  if ! [ -d $OUTPUT_DIR ]; then

    mkdir $OUTPUT_DIR
  
  fi

  ORIGIN_INPUT=$condition/Origin_files_$condition/Origin_TF-genes_$condition.txt

  # Count of the number of replicates of each condition 
  replicates=$(find . -type f -path */$condition/Peaks/* -name '*.narrowPeak' | wc -l)

  for ((i = 1 ; i <= $replicates ; i++));

  do

    OUTPUT_SUBDIR_1=$OUTPUT_DIR/Rep_$i/Network_TF-genes
  
    if ! [ -d $OUTPUT_SUBDIR_1 ]; then

      mkdir -p $OUTPUT_SUBDIR_1
    
    fi
  
    TFBS_INPUT=$condition/TFBS_input_$condition/*/*
           
    # Command to run TOBIAS CreateNetwork
    TOBIAS CreateNetwork --TFBS $TFBS_INPUT --origin $ORIGIN_INPUT --outdir $OUTPUT_SUBDIR_1

  done

done
