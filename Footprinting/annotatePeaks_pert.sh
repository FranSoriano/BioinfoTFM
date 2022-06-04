#!/bin/bash

# Author: Francisco Javier Soriano Díaz

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/Peaks

  OUTPUT_FILE=./$OUTPUT_DIR/$condition'_merged_annotated_peaks.txt'

  OUTPUT_FILE_HEADER=./$OUTPUT_DIR/$condition'_merged_annotated_peaks_header.txt'

  OUTPUT_FILE_WITHOUT_HEADER=./$OUTPUT_DIR/$condition'_merged_annotated_peaks_without_header.txt'

  PEAK_SET=$condition/Peaks/$condition'_merged_peaks.bed'
      
  GENOME=GenomeInfo/mm10_genome.fa
      
  GTF=GenomeInfo/genes.gtf
      
  # Command to run HOMER annotatedPeaks
  annotatePeaks.pl $PEAK_SET $GENOME -gtf $GTF > $OUTPUT_FILE

  # Delete the first column
  cut -f2- $OUTPUT_FILE > tmp1.txt

  # Select the header and replace spaces in header names with "_"
  head -n 1 tmp1.txt | sed '1s/ /_/g' > $OUTPUT_FILE_HEADER

  # Select the lines containing only merged peaks without the header
  tail -n+2 tmp1.txt > $OUTPUT_FILE_WITHOUT_HEADER

  # Remove temporary files
  rm tmp1.txt

done
