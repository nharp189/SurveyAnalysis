### Set wd ###
setwd("/Users/jajolt/Desktop/RScript/")

### load packages ###
library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("year_1.csv", header = TRUE)

### convert character strings to numeric ###
### warning: this replaces all text data with NA ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### analyze Satisfaction with life survey ###


### create SWLS dataframe ###
SWLS <-   survey.data[,c(18, 224:228)]


### add items for participant scores and create column in survey.data ###
SWLS$TOTAL <-  (rowSums(SWLS[,c("Q.SWLS_1", "Q.SWLS_2", "Q.SWLS_3","Q.SWLS_4", "Q.SWLS_5")]))
                            
