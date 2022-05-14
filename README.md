# Enterotype-prediction-based-on-the-life-history
Machine learning classification pipe-line used for the study "Long-term life history predicts current elderly gut microbiome"

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/jorgevazcast/Enterotype-prediction-based-on-the-life-history/blob/main/LICENSE)

## Hardware requirements
The pipe-line was designed to be run into Sun Grid Engine queuing cluster architecture (qsub script.sh), however, it can be run into a desktop computer but the for-loop implementation is extremely inefficient
Processor: Intel® Core™ i7-7820HQ CPU @ 2.90GHz × 8
Memory: 31,0 GiB
OS type: 64-bit

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

