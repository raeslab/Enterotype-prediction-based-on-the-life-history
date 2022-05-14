#!/bin/bash

##############################
### pipe-line selection  #####
piple_line="standar" # Possible values: standar, No_down_sampling, No_feature_selection, and No_down_sampling_no_feature_selection

if [ $piple_line == "standar" ]
then
 nested_cross_validation_rf=./R/nested_cross_validation_rf.R
fi

if [ $piple_line == "No_down_sampling" ]
then
 nested_cross_validation_rf=./R/nested_cross_validation_rf_noDown.R
fi

if [ $piple_line == "No_feature_selection" ]
then
 nested_cross_validation_rf=./R/nested_cross_validation_rf_noFS.R
fi

if [ $piple_line == "No_down_sampling_no_feature_selection" ]
then
 nested_cross_validation_rf=./R/nested_cross_validation_rf_noFS_noDown.R
fi

##########################################
### Run the k-fold cross-validation  #####

# outer_cv_path="./CV_Data/All_years_Rum_CV_40_SCALE_TRUE.RData"
# ent="Rum"  ### Enterotype to use 
# KfoldsInnerLoop=5 ### number of cv in the inner loop
# outdir="All_years_Rum_CV_40_SCALE_TRUE"
# index=38 # num form 1 to 40
# Rscript --vanilla $nested_cross_validation_rf $outer_cv_path $ent $KfoldsInnerLoop $outdir $index # The script must be executed for all the 40 CV sub-datasets


##########################################
######## Run into a for-loop     #########
## IMPORTANT: The pipe-line was designed to be run as an executable script into a batch server (qsub script.sh), the for-loop example is extremely inefficient
filename='parameteres.txt'
while read line; do
 Rscript --vanilla $nested_cross_validation_rf $line
done < $filename

