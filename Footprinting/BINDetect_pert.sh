#!/bin/bash

# Author: Francisco Javier Soriano Díaz

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'BINDetect_'$condition

  if ! [ -d $OUTPUT_DIR ]; then

    mkdir $OUTPUT_DIR
  
  fi
 
  # Input file containing the peak regions of interest and its header
  PEAK_HEADER=$condition/Peaks/$condition'_merged_annotated_peaks_header.txt'
	
  PEAK_SET=$condition/Peaks/$condition'_merged_annotated_peaks_without_header.txt'

  # Count of the number of replicates of each condition 
  replicates=$(find . -type f -path */$condition/Peaks/* -name '*.narrowPeak' | wc -l)

  for ((i = 1 ; i <= $replicates ; i++));

  do
  
    OUTPUT_SUBDIR_1=$OUTPUT_DIR/Rep_$i
  
    if ! [ -d $OUTPUT_SUBDIR_1 ]; then

      mkdir $OUTPUT_SUBDIR_1
    
    fi

    INPUT_SCORES=$condition/ScoreBigwig_$condition/Rep_$i/Rep_$i.$condition'_footprint.bw'
      
    GENOME=GenomeInfo/mm10_genome.fa
      
    MOTIFS=MotifsInfo/filtered_motifs_299.meme
      
    # Command to run TOBIAS BINDetect
    TOBIAS BINDetect --motifs $MOTIFS --signals $INPUT_SCORES --genome $GENOME --peaks $PEAK_SET --peak_header $PEAK_HEADER --outdir $OUTPUT_SUBDIR_1 --cores 8

  done

done
