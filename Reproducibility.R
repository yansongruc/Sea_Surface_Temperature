#===============================================================
#  Sea Surface Temperature Analysis
# [Yan Song]
# May 2020
#===============================================================

#===============================================================
# Reproducibility: MASTER script
#===============================================================

# Please download the repository from GitHub [https://github.com/yansongruc/Sea_Surface_Temperature].
# The following script assumes the working directory has been set to this folder. 
# Raw data, treated data and all code are now available.

# This file contains instructions for:
# Step 1. Take exploratory data analysis on the raw data SST.mat.
# Step 2. Introduce subsampling methods, including random, deep and wide and MaxProLHD.
# Step 3. Compare the performance of various subsamples in parameter estimation and prediction.
# Step 4. Fill in the missing values with subsamples with best performance in step 3.

#===============================================================
# Step 0: Install necessary packages and download the raw data
#===============================================================

## Necessary Packages
library("R.matlab")
library("fields")
library("geoR")
library("MaxPro")
library("ggplot2")
library("doParallel")
library("foreach")

## Download the raw data data_SST.mat.
setwd("../Data")
SST=readMat("data_SST.mat",maxLength = NULL,fixNames = TRUE)

#===============================================================
# Step 1: Exploratory Data Analysis 
#===============================================================

setwd("../Codes")
source("1_proposal.R")

#===============================================================
# Step 2: Generate various subsamples
#===============================================================

# Since the target of the project is filling in missing values of Day 10,
# we use the treated data SST_10.csv in following steps.

setwd("../Data")
Data0=read.csv("SST_10.csv")[,-1]

setwd("../Codes")
source("2_various_subsamples.R")

#===============================================================
# Step 3: Compare various subsampling methods
#===============================================================

#setwd("../Data")
#Data0=read.csv("SST_10.csv")[,-1]

setwd("../Codes")
source("3_compare_subsamples.R")

# summary the results and plot figures 

source("4_Results_treatment.R")

#===============================================================
# Step 4: Fill in
#===============================================================

setwd("../Codes")
source("5_Fill_in.R")