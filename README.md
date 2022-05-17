# Enterotype-prediction-based-on-the-life-history
Machine learning classification pipe-line used for the study "Long-term life history predicts current elderly gut microbiome"

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/jorgevazcast/Enterotype-prediction-based-on-the-life-history/blob/main/LICENSE)

## Hardware requirements
The pipe-line is designed to be run into Sun Grid Engine queuing cluster architecture (qsub script.sh), however, it can be run into a desktop computer but the for-loop implementation is extremely inefficient
(Processor: Intel® Core™ i7-7820HQ CPU @ 2.90GHz × 8,
Memory: 31 GiB,
OS type: 64-bit)

## Software requirements
###### R version 4.0.3 (2020-10-10)
R packages:
* caret_6.0-86
* DMwR_0.4.1
* ROSE_0.0-4
* psych_2.1.9
* MLeval_0.3
* pROC_1.17.0.1
* mltools_0.3.5

## Run the pipeline
###### Ubuntu bash script
Run a single CV 
```
nested_cross_validation_rf=./R/nested_cross_validation_rf.R
outer_cv_path="./CV_Data/All_years_Rum_CV_40_SCALE_TRUE.RData"
ent="Rum"  ### Enterotype
KfoldsInnerLoop=5 ### number of cv in the inner loop
outdir="All_years_Rum_CV_40_SCALE_TRUE"
index=38 # num form 1 to 40

Rscript --vanilla $nested_cross_validation_rf $outer_cv_path $ent $KfoldsInnerLoop $outdir $index # The script must be executed for all the 40 CV sub-datasets
```
Run into a for loop. IMPORTANT: The pipeline was designed to be run as an executable script into a batch server (qsub script.sh), the for loop example is extremely inefficient
```
filename='parameters.txt'
while read line; do
 Rscript --vanilla $nested_cross_validation_rf $line
done < $filename
```

