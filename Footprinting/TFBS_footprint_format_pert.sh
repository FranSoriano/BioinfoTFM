#!/bin/bash

# Author: Francisco Javier Soriano Díaz

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'TFBS_input_'$condition

  if ! [ -d $OUTPUT_DIR ]; then

    mkdir $OUTPUT_DIR
  
  fi

  # Count of the number of replicates of each condition 
  replicates=$(find . -type f -path */$condition/Peaks/* -name '*.narrowPeak' | wc -l)

  for ((i = 1 ; i <= $replicates ; i++));

  do
  
    OUTPUT_SUBDIR_1=$OUTPUT_DIR/Rep_$i
  
    if ! [ -d $OUTPUT_SUBDIR_1 ]; then

      mkdir $OUTPUT_SUBDIR_1
    
    fi

    INPUT_BED_FILES=$condition/Selected_acinar_expressed_TFs_$condition/Rep_$i/*

    for file in $INPUT_BED_FILES
    
    do
      
      # Selection of columns of interest:
      # Names of the TF with the first letter in uppercase followed
      # by lowercase  
      awk -F"\t" '{print $4}' $file | cut -d "_" -f 1 | sed -e 's/.*/\L&/' -e 's/\w/\u&/' > tmp1.txt

      # Chr and start-end coordinates
      awk -F"\t" '{print $1"\t"$2"\t"$3}' $file > tmp2.txt

      # TFBS-Scores, target genes IDs, target genes names, regions and footprint-scores
      #(all the sites from the "bound" file has passed the footprint-score threshold established
      # to be considered as bound by the corresponding TF)
      awk -F"\t" '{print $5"\t"$13"\t"$16"\t"$17"\t"$25}' $file > tmp3.txt
      
      names=$(cat tmp1.txt)
	
      coordinates=$(cat tmp2.txt)
	
      annotations=$(cat tmp3.txt)

      # Generate a unique BED file with the name of the corresponding TF in the
      # specified output directory
      paste <(echo "$coordinates") <(echo "$names") <(echo "$annotations") > $OUTPUT_SUBDIR_1"/$(head -1 tmp1.txt).bed"
      rm tmp*.txt
      
    done

  done

done
