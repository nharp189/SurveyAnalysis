### Set wd ###
setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/longitudinal_study/data/R_Analysis")

### install.packages ###
#install.packages("car")

### load packages ###
library(tidyverse)
library(psych)
library(car)

### read in the data ###
survey.data <- read.csv("fall18subs_qual_y1.csv", header = TRUE)

### convert character strings to numeric ###
### warning: this replaces all text data with NA ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### analyze Beck Depression Inventory ###

### create BDI dataframe ###
BDI <-   survey.data[,c(18, 229:249)]

### re-code 1, 2, 3, 4 into 0, 1, 2, 3 for b.1 through b.15###
BDI[ , 2:16] <- lapply(BDI[ , 2:16], FUN = function(x) recode(x, ("1=0; 2=1; 3=2; 4=3")))

### re-code 0, 1a, 1b, 2a, 2b questions ###
BDI$b.16 <- ifelse(BDI$b.16 == 1, 0,
                   ifelse(BDI$b.16 == 2, 1,
                          ifelse(BDI$b.16 == 3, 1,
                                 ifelse(BDI$b.16 == 4, 2,
                                        ifelse(BDI$b.16 == 5, 2,
                                               ifelse(BDI$b.16 == 6, 3,
                                                      ifelse(BDI$b.16 == 7, 3, NA)))))))

### re-code 0, 1a, 1b, 2a, 2b questions ###
BDI$b.18 <- ifelse(BDI$b.18 == 1, 0,
                   ifelse(BDI$b.18 == 2, 1,
                          ifelse(BDI$b.18 == 3, 1,
                                 ifelse(BDI$b.18 == 4, 2,
                                        ifelse(BDI$b.18 == 5, 2,
                                               ifelse(BDI$b.18 == 6, 3,
                                                      ifelse(BDI$b.18 == 7, 3, NA)))))))

### re-code 1, 2, 3, 4 into 0, 1, 2, 3 for b.17 and b.19 through b.21###
BDI[ , 2:16] <- lapply(BDI[ , c(18, 20:22)], FUN = function(x) recode(x, ("1=0; 2=1; 3=2; 4=3")))


### add items for participant scores and create column in survey.data ###
survey.data$BDI <- rowSums(BDI[,c(2:22)], na.rm = FALSE) 

### add items for participant scores and create column in BDI ###
BDI$BDI <- rowSums(BDI[,c(2:22)], na.rm = TRUE) 

### rename Q1 to subjID for merging###
colnames(BDI)[colnames(BDI)=="Q1"] <- "subjID" 

### merge with other data ###
combined <- merge(BDI, combined, by= "subjID")

write.csv(combined, file = "combined_data.csv")

  

