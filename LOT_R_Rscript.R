### Set wd ###
setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/longitudinal_study/data/R_Analysis")

### load packages ###
library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("fall18subs_qual_y1.csv", header = TRUE)

### convert character strings to numeric ###
### warning: this replaces all text data with NA ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### analyze Life Orientation Test-Revised ###
### items 2, 5, 6, and 8 are fillers ###
survey.data$Q.LOT_2 <- NULL
survey.data$Q.LOT_5 <- NULL
survey.data$Q.LOT_6 <- NULL 
survey.data$Q.LOT_8 <- NULL 

### create LOT_R dataframe ###
LOT_R <-   survey.data[,c(18, 30:35)]

### code for reverse scoring (-1 = reverse) ###
LOT_R.keys <- c(1, 1, -1, 1, -1, -1, 1)

### (reverse) score items ###
LOT_R.scored <- reverse.code(LOT_R.keys, LOT_R, mini = 1, maxi = 5)

### add items for participant scores and create column in survey.data ###
survey.data$LOT_R <- rowSums(LOT_R[,c("Q.LOT_1", "Q.LOT_3", "Q.LOT_4", 
                                      "Q.LOT_7", "Q.LOT_9", "Q.LOT_10")], 
                                      na.rm = TRUE) 

### add items for participant scores and create column in LOT_R ###
LOT_R$LOT_R <- rowSums(LOT_R[,c("Q.LOT_1", "Q.LOT_3", "Q.LOT_4", 
                                      "Q.LOT_7", "Q.LOT_9", "Q.LOT_10")], 
                             na.rm = TRUE) 




  

