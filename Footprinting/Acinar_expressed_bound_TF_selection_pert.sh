#!/bin/bash

# Author: Francisco Javier Soriano Díaz

SELECTED_TF=MotifsInfo/list_of_299_motif_names.txt 

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'Selected_acinar_expressed_TFs_'$condition

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

    # Input directory with the respective subdirectories for each TF
    INPUT_DIR=$condition/BINDetect_$condition/Rep_$i
    
    echo "Searching for bound.bed files for the specified TFs in BINDetect_"$condition"/Rep_"$i
    echo "It might take some minutes..."
    
    while read -r FILE
    
    do
    
      find $INPUT_DIR -iname "$FILE""_*_bound.bed" -exec cp {} $OUTPUT_SUBDIR_1 \;
      
    done < $SELECTED_TF # Corresponds to $FILE and it is read to find matches
    
    if [ $? -ne 0 ]; then
    
      echo "Files could not be copied"
      
      else
      
      echo "Files copied successfully!"
      
    fi

  done

done
