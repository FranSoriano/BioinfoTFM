#!/bin/bash

# Author: Francisco Javier Soriano Díaz


for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'ATACorrect_'$condition

  if ! [ -d $OUTPUT_DIR ]; then

    mkdir $OUTPUT_DIR
  
  fi

  PEAK_SET=$condition/Peaks/$condition'_merged_peaks.bed'

  # Count of the number of replicates of each condition 
  replicates=$(find . -type f -path */$condition/Bams/* -name '*.bam' | wc -l)

  for ((i = 1 ; i <= $replicates ; i++));

  do
  
    OUTPUT_SUBDIR_1=$OUTPUT_DIR/Rep_$i
  
    if ! [ -d $OUTPUT_SUBDIR_1 ]; then

      mkdir $OUTPUT_SUBDIR_1
    
    fi
      
     INPUT_READS=$condition/Bams/Rep_$i.$condition."*".trim.srt.nodup.no_chrM_MT.bam
      
     GENOME=GenomeInfo/mm10_genome.fa
      
     BLACKLIST=GenomeInfo/mm10.blacklist.bed
      
     # Command to run TOBIAS ATACorrect
     TOBIAS ATACorrect --bam $INPUT_READS --genome $GENOME --peaks $PEAK_SET --blacklist $BLACKLIST --outdir $OUTPUT_SUBDIR_1 --cores 8
  
  done

done
