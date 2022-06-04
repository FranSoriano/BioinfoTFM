#!/bin/bash

# Author: Francisco Javier Soriano Díaz

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'ScoreBigwig_'$condition

  if ! [ -d $OUTPUT_DIR ]; then

    mkdir $OUTPUT_DIR
  
  fi
  
  PEAK_SET=$condition/Peaks/$condition'_merged_peaks.bed'

  # Count of the number of replicates of each condition 
  replicates=$(find . -type f -path */$condition/Peaks/* -name '*.narrowPeak' | wc -l)

  for ((i = 1 ; i <= $replicates ; i++));

  do
  
    OUTPUT_SUBDIR_1=$OUTPUT_DIR/Rep_$i
  
    if ! [ -d $OUTPUT_SUBDIR_1 ]; then

      mkdir $OUTPUT_SUBDIR_1
    
    fi
      
    INPUT_SIGNAL=$condition/ATACorrect_$condition/Rep_$i/Rep_$i.$condition."*".trim.srt.nodup.no_chrM_MT_corrected.bw
      
    OUTPUT_FILE=$OUTPUT_SUBDIR_1/Rep_$i.$condition'_footprint.bw'
      
    # Command to run TOBIAS ScoreBigwig 
    TOBIAS ScoreBigwig --signal $INPUT_SIGNAL --regions $PEAK_SET --output $OUTPUT_FILE --cores 8
  
  done
  
done
