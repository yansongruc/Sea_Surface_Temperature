# Sea_Surface_Temperature
## Background
West-blowing trade winds in the Indian Ocean push warm surface waters against the eastern coast of Africa. These waters move south along the coastline, eventually spilling out along the boundary of the Indian and Atlantic Oceans. This jet of warm water, known as the Agulhas Current, collides with the cold, west to east flowing Antarctic Circumpolar Current, producing a dynamic series of meanders and eddies as the two waters mix. 

### Data description and avalibility
The data SST.mat file contains sea surface temperature data collected by satellite for the Agulhas and surrounding areas off the coast of South Africa from January 1 to November 26, 2004, a period of 331 days. The data contains a lot of missing values which caused by land, satellite’s orbital clipping and cloud cover.

The raw data SST.mat is available for download via GitHub [https://github.com/yansongruc/Sea_Surface_Temperature/Data].

## Project 
In this project, we aim to fill in the missing values present in Day 10 data by Gaussian geostatistical model. Due to computational limitation, we consider to replace the full data with subsamples. The project is organized as follows:

Step 1. Take exploratory data analysis on the raw data SST.mat.

Step 2. Introduce subsampling methods, including random, deep and wide and MaxProLHD.

Step 3. Compare the performance of various subsamples in parameter estimation and prediction.

Step 4. Fill in the missing values with subsamples with best performance in step 3.

### Reproducibility
Reproduction process is in Reproducibility.R script. 

**1_proposal.R** is used to analyze the raw data and choose a proper fitting model.

**2_various_subsamples.R** is used to select various subsamples and display them.

**3_compare_subsamples.R** is used in Step 3. We first generate $m$ subsamples by various methods. Then we use them to estimate the prameters and make a prediction. More details can be found at Final_Report.

**4_Results_treatment.R** is used to summary the results and display them.

**5_Fill_in.R** is used to fill in the missing values with best subsamples.

### Optional Information
R version 3.6.1 (2019-07-05) was used for the analyses in this report. The necessary R libraries for the code used for data processing and analysis are:
 * R.matlab, version 3.6.2 (https://CRAN.R-project.org/package=R.matlab)
 * fields, version 9.8-6 (https://CRAN.R-project.org/package=fields)
 * geoR, version 1.7-5.2.1 (https://CRAN.R-project.org/package=geoR)
 * MaxPro, version 4.1-2 (https://CRAN.R-project.org/package=MaxPro)
 * ggplot2, version 3.2.1 (https://CRAN.R-project.org/package=ggplot2)
 * doParallel, version 1.0.15 (https://CRAN.R-project.org/package=doParallel)
 * foreach, version 1.4.7 (https://CRAN.R-project.org/package=foreach)
