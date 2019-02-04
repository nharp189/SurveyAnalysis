### Set wd ###
setwd("/Users/jajolt/Desktop/RScript/")

install.packages("tidyverse")
install.packages("psych")
### load packages ###

library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("/Users/jajolt/Desktop/RScript/year_1.csv", header = TRUE)

### analyze IUS ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### create IUS dataframe ###
IUS <-survey.data[,c(18, 207:218)]
IUS.matrix <- as.matrix(IUS)
### no reverse scoring for IUS ###
### score items ###
IUS$TOTAL <-  (rowSums(IUS[,c("Q.IUS_1", "Q.IUS_2", "Q.IUS_3","Q.IUS_4", "Q.IUS_5","Q.IUS_6","Q.IUS_7","Q.IUS_8","Q.IUS_9","Q.IUS_10","Q.IUS_11","Q.IUS_12")]))

