### Set wd ###
setwd("/Users/jajolt/Desktop/RScript/")

install.packages("tidyverse")
install.packages("psych")
### load packages ###

library(tidyverse)
library(psych)

### read in the data ###
survey.data <- read.csv("/Users/jajolt/Desktop/RScript/year_1.csv", header = TRUE)

### analyze Interpersonal Regulation Questionnaire ###
survey.data[] <- lapply(survey.data, function(x) as.numeric(as.character(x)))

### create IRQ dataframe ###
IRQ <-survey.data[,c(18, 40:55)]
IRQNT <-survey.data[,c(18, 40:43)]
IRQNE <-survey.data[,c(18, 44:47)]
IRQPT <-survey.data[,c(18, 48:51)]
IRQPE <-survey.data[,c(18, 52:55)]
IRQ.matrix <- as.matrix(IRQ)
IRQNT.matrix <- as.matrix(IRQNT)
IRQNE.matrix <- as.matrix(IRQNE)
IRQPT.matrix <- as.matrix(IRQPT)
IRQPE.matrix <- as.matrix(IRQPE)
### no reverse scoring for IRQ ###
### score items ###
IRQ$TOTAL <-  (rowSums(IRQ[,c("Q.IRQ_1", "Q.IRQ_2", "Q.IRQ_3","Q.IRQ_4", "Q.IRQ_5","Q.IRQ_6","Q.IRQ_7","Q.IRQ_8","Q.IRQ_9","Q.IRQ_10","Q.IRQ_11","Q.IRQ_12","Q.IRQ_13","Q.IRQ_14","Q.IRQ_15")]))
IRQNT$TOTAL <- (rowSums(IRQNT[,c("Q.IRQ_1", "Q.IRQ_2", "Q.IRQ_3","Q.IRQ_4")]))
IRQNE$TOTAL <- (rowSums(IRQNE[,c("Q.IRQ_5", "Q.IRQ_6", "Q.IRQ_7","Q.IRQ_8")]))
IRQPT$TOTAL <- (rowSums(IRQPT[,c("Q.IRQ_9", "Q.IRQ_10", "Q.IRQ_11","Q.IRQ_12")]))
IRQPE$TOTAL <- (rowSums(IRQPE[,c("Q.IRQ_13", "Q.IRQ_14", "Q.IRQ_15","Q.IRQ_16")]))
