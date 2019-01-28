### Set wd ###
setwd("/Users/jajolt/Desktop/RScript/")

### load packages ###
library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("fall18subs_qual_y1.csv", header = TRUE)

### convert character strings to numeric ###
### warning: this replaces all text data with NA ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### analyze EMotion Regulation Ques ###


### create ERQ dataframe ###
ERQ <-   survey.data[,c(18, 183:192)]


### add items for participant scores and create column in survey.data ###
ERQ$ERQ_CR <-  (rowSums(ERQ[,c("Q.ERQ_1", "Q.ERQ_3", "Q.ERQ_5", 
                                      "Q.ERQ_7", "Q.ERQ_8", "Q.ERQ_10")], 
                             na.rm = FALSE)  / 6)

ERQ$ERQ_ES <- ( rowSums(ERQ[,c("Q.ERQ_2", "Q.ERQ_4", "Q.ERQ_6", 
                                     "Q.ERQ_9")], 
                              na.rm = FALSE) / 4 )
