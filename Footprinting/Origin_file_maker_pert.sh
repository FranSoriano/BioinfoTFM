#!/bin/bash

# Author: Francisco Javier Soriano Díaz

SELECTED_TF=MotifsInfo/list_of_299_motif_names.txt

TF_NAME_ID_LIST=MotifsInfo/Selected_TF_name_ID_299.txt

for condition in Cae_Kras Cae_Kras_Gata4KO NT_Gata4KO NT_Gata6KO NT_p48Cre PBS_Kras PBS_Kras_Gata4KO

do

  OUTPUT_DIR=$condition/'Origin_files_'$condition

  if ! [ -d $OUTPUT_DIR ]; then

    mkdir $OUTPUT_DIR
  
  fi

  INPUT_TFBS=$condition/TFBS_input_$condition/*/*

  OUTPUT_FILE_TF_TF=$OUTPUT_DIR/Origin_TF-TF_$condition.txt

  OUTPUT_FILE_TF_ALL=$OUTPUT_DIR/Origin_TF-genes_$condition.txt

  for file in $INPUT_TFBS
  
  do

    # Selection of columns of interest:
    # Target genes names and target genes IDs  
    awk -F"\t" '{print $8"\t"$7}' $file >> tmp1.txt

  done

  sort tmp1.txt | uniq > tmp2.txt
  
  # The input TFs must be present in the origin file, but there can be some not
  # being regulated by other TFs, therefore we have to add their name-ID to the
  # origin file
  cat tmp2.txt $TF_NAME_ID_LIST | sort | uniq > $OUTPUT_FILE_TF_ALL

  # Filter the origin file to select only TFs for TF-TF network building
  # NR==FNR: NR is the current input line number and FNR the current file's line
  #          number. The two will be equal only while the 1st file is being read.
  awk -F'\t' 'NR==FNR{c[$1]++;next};c[$1] > 0' $SELECTED_TF $OUTPUT_FILE_TF_ALL > $OUTPUT_FILE_TF_TF

  rm tmp*.txt

done
