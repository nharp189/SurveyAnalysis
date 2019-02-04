### Set wd ###
setwd("/Users/jajolt/Desktop/RScript/")

install.packages("tidyverse")
install.packages("psych")
### load packages ###

library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("/Users/jajolt/Desktop/RScript/year_1.csv", header = TRUE)

### analyze Stielberger’s State-Trait Anxiety Inventory ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### create IRQ dataframe ###
STAIS <-survey.data[,c(18, 56:75)]
STAIT <-survey.data[,c(18, 76:95)]
STAIS.matrix <- as.matrix(STAIS)
STAIT.matrix <- as.matrix(STAIT)
### no reverse scoring for STAIS ###
### score items ###
STAIS$TOTAL <-  (rowSums(STAIS[,c("Q.STAIS_1", "Q.STAIS_2", "Q.STAIS_3","Q.STAIS_4", "Q.STAIS_5","Q.STAIS_6","Q.STAIS_7","Q.STAIS_8","Q.STAIS_9","Q.STAIS_10","Q.STAIS_11","Q.STAIS_12","Q.STAIS_13","Q.STAIS_14","Q.STAIS_15","Q.STAIS_16","Q.STAIS_17","Q.STAIS_18","Q.STAIS_19","Q.STAIS_20")]))
STAIT$TOTAL <-  (rowSums(STAIT[,c("Q.STAIT_1", "Q.STAIT_2", "Q.STAIT_3","Q.STAIT_4", "Q.STAIT_5","Q.STAIT_6","Q.STAIT_7","Q.STAIT_8","Q.STAIT_9","Q.STAIT_10","Q.STAIT_11","Q.STAIT_12","Q.STAIT_13","Q.STAIT_14","Q.STAIT_15","Q.STAIT_16","Q.STAIT_17","Q.STAIT_18","Q.STAIT_19","Q.STAIT_20")]))
