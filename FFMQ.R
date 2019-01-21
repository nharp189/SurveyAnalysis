### Set wd ###
setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/longitudinal_study/data/R_Analysis")

install.packages("multivariate")
### load packages ###
library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("fall18subs_qual_y1.csv", header = TRUE)

### convert character strings to numeric ###
### warning: this replaces all text data with NA ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### analyze Five Facet Mindfulness Questionnaire ###

### create FFMQ dataframe ###
FFMQ <-   survey.data[,c(18, 120:158)]

### Q1 = subjID
colnames(FFMQ)[colnames(FFMQ)=="Q1"] <- "subjID"

### code for reverse scoring (-1 = reverse) ###
FFMQ.keys <- c(1, 1, 1, -1, 1, -1, 1, 1, -1, 1, -1,
               1, -1, -1, -1, 1, -1, -1, -1, 1, 1, 
               1, -1, -1, 1, -1, 1, 1, -1, 1, -1, 1, 
               1, 1, -1, -1, 1, 1, -1, -1)

### (reverse) score items ###
FFMQ.scored <- reverse.code(FFMQ.keys, FFMQ, mini = 1, maxi = 5)

### calculate factor scores ###
### observe ###
FFMQ$FFMQ_Ob <- rowSums(FFMQ[,c("FFMQ_1", "FFMQ_6", "FFMQ_11", 
                                 "FFMQ_15", "FFMQ_20", "FFMQ_26",
                                 "FFMQ_31", "FFMQ_36")], na.rm = FALSE) 

### describe ###
FFMQ$FFMQ_Ds <- rowSums(FFMQ[,c("FFMQ_2", "FFMQ_7", "FFMQ_12", 
                                "FFMQ_16", "FFMQ_22", "FFMQ_27",
                                "FFMQ_32", "FFMQ_37")], na.rm = FALSE) 

### act with awareness ###
FFMQ$FFMQ_Aw <- rowSums(FFMQ[,c("FFMQ_5", "FFMQ_8", "FFMQ_13", 
                                "FFMQ_18", "FFMQ_23", "FFMQ_28",
                                "FFMQ_34", "FFMQ_38")], na.rm = FALSE) 

### nonjudge ###
FFMQ$FFMQ_Nj <- rowSums(FFMQ[,c("FFMQ_3", "FFMQ_10", "FFMQ_14", 
                                "FFMQ_17", "FFMQ_25", "FFMQ_30",
                                "FFMQ_35", "FFMQ_39")], na.rm = FALSE) 

### nonreact ###
FFMQ$FFMQ_Nr <- rowSums(FFMQ[,c("FFMQ_4", "FFMQ_9", "FFMQ_19", 
                                "FFMQ_21", "FFMQ_24", "FFMQ_29",
                                "FFMQ_33")], na.rm = FALSE) 


### add FFMQ back to survey.data ###
survey.data$FFMQ_Ob <- FFMQ$FFMQ_Ob 
survey.data$FFMQ_Ds <- FFMQ$FFMQ_Ds 
survey.data$FFMQ_Aw <- FFMQ$FFMQ_Aw 
survey.data$FFMQ_Nj <- FFMQ$FFMQ_Nj 
survey.data$FFMQ_Nr <- FFMQ$FFMQ_Nr  
  
### check cronbach's alpha ###
FFMQ.akeys.Ob <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
               1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
               0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
               0, 0, 0, 0, 1, 0, 0, 0)
psych::alpha(FFMQ, keys = FFMQ.akeys.Ob)

FFMQ.akeys.Ds <- c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
                   0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 
                   0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                   1, 0, 0, 0, 0, 1, 0, 0)
psych::alpha(FFMQ, keys = FFMQ.akeys.Ds)


### still need to do other 3 factors ###
