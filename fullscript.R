### Set wd ###
setwd("/Users/jajolt/Desktop/RScript/")

install.packages("tidyverse")
install.packages("psych")
install.packages("data.table")
### load packages ###

library(tidyverse)
library(psych)
library(data.table)

### read in the data ###
survey.data <- read.csv("/Users/jajolt/Desktop/RScript/year_1.csv", header = TRUE)

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

### analyze Interpersonal Regulation Questionnaire ###
### create IRQ dataframe ###
IRQ <-survey.data[,c(18, 36:51)]
IRQNT <-survey.data[,c(18, 36:39)]
IRQNE <-survey.data[,c(18, 40:43)]
IRQPT <-survey.data[,c(18, 44:47)]
IRQPE <-survey.data[,c(18, 48:51)]
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

### analyze Stielberger’s State-Trait Anxiety Inventory ###

### create IRQ dataframe ###
STAIS <-survey.data[,c(18, 52:71)]
STAIT <-survey.data[,c(18, 72:91)]
STAIS.matrix <- as.matrix(STAIS)
STAIT.matrix <- as.matrix(STAIT)
### no reverse scoring for STAIS ###
### score items ###
STAIS$TOTAL <-  (rowSums(STAIS[,c("Q.STAIS_1", "Q.STAIS_2", "Q.STAIS_3","Q.STAIS_4", "Q.STAIS_5","Q.STAIS_6","Q.STAIS_7","Q.STAIS_8","Q.STAIS_9","Q.STAIS_10","Q.STAIS_11","Q.STAIS_12","Q.STAIS_13","Q.STAIS_14","Q.STAIS_15","Q.STAIS_16","Q.STAIS_17","Q.STAIS_18","Q.STAIS_19","Q.STAIS_20")]))
STAIT$TOTAL <-  (rowSums(STAIT[,c("Q.STAIT_1", "Q.STAIT_2", "Q.STAIT_3","Q.STAIT_4", "Q.STAIT_5","Q.STAIT_6","Q.STAIT_7","Q.STAIT_8","Q.STAIT_9","Q.STAIT_10","Q.STAIT_11","Q.STAIT_12","Q.STAIT_13","Q.STAIT_14","Q.STAIT_15","Q.STAIT_16","Q.STAIT_17","Q.STAIT_18","Q.STAIT_19","Q.STAIT_20")]))

### analyze Five Facet Mindfulness Questionnaire ###

### create FFMQ dataframe ###
FFMQ <-   survey.data[,c(18, 116:154)]
FFMQ.matrix <- as.matrix(FFMQ)
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


### analyze EMotion Regulation Ques ###


### create ERQ dataframe ###
ERQ <-   survey.data[,c(18, 183:192)]
ERQ.matrix <- as.matrix(ERQ)

### add items for participant scores and create column in survey.data ###
ERQ$ERQ_CR <-  (rowSums(ERQ[,c("Q.ERQ_1", "Q.ERQ_3", "Q.ERQ_5", 
                               "Q.ERQ_7", "Q.ERQ_8", "Q.ERQ_10")], 
                        na.rm = FALSE)  / 6)

ERQ$ERQ_ES <- ( rowSums(ERQ[,c("Q.ERQ_2", "Q.ERQ_4", "Q.ERQ_6", 
                               "Q.ERQ_9")], 
                        na.rm = FALSE) / 4 )



### create IUS dataframe ###
IUS <-survey.data[,c(18, 203:214)]
IUS.matrix <- as.matrix(IUS)
### no reverse scoring for IUS ###
### score items ###
IUS$TOTAL <-  (rowSums(IUS[,c("Q.IUS_1", "Q.IUS_2", "Q.IUS_3","Q.IUS_4", "Q.IUS_5","Q.IUS_6","Q.IUS_7","Q.IUS_8","Q.IUS_9","Q.IUS_10","Q.IUS_11","Q.IUS_12")]))


### analyze Satisfaction with life survey ###


### create SWLS dataframe ###
SWLS <-   survey.data[,c(18, 220:224)]
SWLS.matrix <- as.matrix(SWLS)


### add items for participant scores and create column in survey.data ###
SWLS$TOTAL <-  (rowSums(SWLS[,c("Q.SWLS_1", "Q.SWLS_2", "Q.SWLS_3","Q.SWLS_4", "Q.SWLS_5")]))

###Bring demographics back as binary values at the end###
###Sex: 1 for male, 2 for female###
Sex<- survey.data[,c(20)]
survey.data$Sex <-Sex
###Race: Native American 1, Asian 2, Hawaiian/Pacific Islander 3, Black/African American 4, White 5, More than one race 6, Asian 7(?)###
Race<- survey.data[,c(21)]
survey.data$Race <-Race
###Ethnicity: 1: Hispanic or Latino. 2: Not hispanic or latino###
Ethnicity <- survey.data[,c(22)]
survey.data$Ethnicity <-Ethnicity
###Age###
Age <- survey.data[,c(19)]
survey.data$Age <-Age
###PsychDisorders: 1 for yes, 2 for no###
Psych <- survey.data[,c(23)]
survey.data$PsychologicalDisorders <- Psych
###Class: 1 for freshman, 2 for sophomore, 3 for junior, 4 for senior###
Class <- survey.data[,c(26)]
survey.data$Class <-Class
###Residence: 1 for Residence Hall, 2 for Off-campus housing, 3 for frat/sorority, 4 for home###
survey.data$Residence <- survey.data[,c(27)]

###Scores at end###
survey.data$IRQ <-  (rowSums(IRQ[,c("Q.IRQ_1", "Q.IRQ_2", "Q.IRQ_3","Q.IRQ_4", "Q.IRQ_5","Q.IRQ_6","Q.IRQ_7","Q.IRQ_8","Q.IRQ_9","Q.IRQ_10","Q.IRQ_11","Q.IRQ_12","Q.IRQ_13","Q.IRQ_14","Q.IRQ_15")]))
survey.data$IRQNT <- (rowSums(IRQNT[,c("Q.IRQ_1", "Q.IRQ_2", "Q.IRQ_3","Q.IRQ_4")]))
survey.data$IRQNE <- (rowSums(IRQNE[,c("Q.IRQ_5", "Q.IRQ_6", "Q.IRQ_7","Q.IRQ_8")]))
survey.data$IRQPT <- (rowSums(IRQPT[,c("Q.IRQ_9", "Q.IRQ_10", "Q.IRQ_11","Q.IRQ_12")]))
survey.data$IRQPE <- (rowSums(IRQPE[,c("Q.IRQ_13", "Q.IRQ_14", "Q.IRQ_15","Q.IRQ_16")]))
survey.data$STAIS <-  (rowSums(STAIS[,c("Q.STAIS_1", "Q.STAIS_2", "Q.STAIS_3","Q.STAIS_4", "Q.STAIS_5","Q.STAIS_6","Q.STAIS_7","Q.STAIS_8","Q.STAIS_9","Q.STAIS_10","Q.STAIS_11","Q.STAIS_12","Q.STAIS_13","Q.STAIS_14","Q.STAIS_15","Q.STAIS_16","Q.STAIS_17","Q.STAIS_18","Q.STAIS_19","Q.STAIS_20")]))
survey.data$STAIT <-  (rowSums(STAIT[,c("Q.STAIT_1", "Q.STAIT_2", "Q.STAIT_3","Q.STAIT_4", "Q.STAIT_5","Q.STAIT_6","Q.STAIT_7","Q.STAIT_8","Q.STAIT_9","Q.STAIT_10","Q.STAIT_11","Q.STAIT_12","Q.STAIT_13","Q.STAIT_14","Q.STAIT_15","Q.STAIT_16","Q.STAIT_17","Q.STAIT_18","Q.STAIT_19","Q.STAIT_20")]))
survey.data$FFMQ_Ob <- FFMQ$FFMQ_Ob 
survey.data$FFMQ_Ds <- FFMQ$FFMQ_Ds 
survey.data$FFMQ_Aw <- FFMQ$FFMQ_Aw 
survey.data$FFMQ_Nj <- FFMQ$FFMQ_Nj 
survey.data$FFMQ_Nr <- FFMQ$FFMQ_Nr  
survey.data$ERQ_CR <-  (rowSums(ERQ[,c("Q.ERQ_1", "Q.ERQ_3", "Q.ERQ_5", 
                               "Q.ERQ_7", "Q.ERQ_8", "Q.ERQ_10")], 
                        na.rm = FALSE)  / 6)
survey.data$ERQ_ES <- ( rowSums(ERQ[,c("Q.ERQ_2", "Q.ERQ_4", "Q.ERQ_6", 
                               "Q.ERQ_9")], 
                        na.rm = FALSE) / 4   )
survey.data$IUS <-  (rowSums(IUS[,c("Q.IUS_1", "Q.IUS_2", "Q.IUS_3","Q.IUS_4", "Q.IUS_5","Q.IUS_6","Q.IUS_7","Q.IUS_8","Q.IUS_9","Q.IUS_10","Q.IUS_11","Q.IUS_12")]))
survey.data$SWLS <- (rowSums(SWLS[,c("Q.SWLS_1", "Q.SWLS_2", "Q.SWLS_3","Q.SWLS_4", "Q.SWLS_5")]))


###Merge scores with valence bias###
valence.bias <- read.csv("/Users/jajolt/Desktop/RScript/ratings.csv", header = TRUE)
setnames(survey.data, old=c("Q1"), new=c("subjID"))
view(survey.data$subjID)
questionnaire.data = merge(survey.data, valence.bias)
write.csv(questionnaire.data,'combinedscores.csv')


### create val_rate dataframe, average IAPS with faces for total val_rate ###
questionnaire.data$val_rate <- rowMeans(questionnaire.data[c('sur_rate', 'amb_rate')], na.rm=TRUE)


###         Plots         ###


###     IRQ     ###

###Plot questionnaire.data against IRQ###
pdf("plots.pdf")
plot(questionnaire.data$IRQ, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with IRQ Score",
     ylab = "Valence Score",
     xlab = "IRQ Score" )
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$IRQ))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$IRQ)

###Plot questionnaire.data against IRQNT###
plot(questionnaire.data$IRQNT, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with IRQNT Score",
     ylab = "Valence Score",
     xlab = "IRQNT Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$IRQNT))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$IRQNT)

###Plot questionnaire.data against IRQNE###
plot(questionnaire.data$IRQNE, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with IRQNE Score",
     ylab = "Valence Score",
     xlab = "IRQNE Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$IRQNE))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$IRQNE)

###Plot questionnaire.data against IRQPT###
plot(questionnaire.data$IRQPT, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with IRQPT Score",
     ylab = "Valence Score",
     xlab = "IRQPT Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$IRQPT))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$IRQPT)

###Plot questionnaire.data against IRQPE###
plot(questionnaire.data$IRQPE, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with IRQPE Score",
     ylab = "Valence Score",
     xlab = "IRQPE Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$IRQPE))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$IRQPE)

###     STAIS/STAIT     ###
###Plot questionnaire.data against STAIS###
plot(questionnaire.data$STAIS, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with STAIS Score",
     ylab = "Valence Score",
     xlab = "STAIS Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$STAIS))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$STAIS)

###Plot questionnaire.data against STAIT###
plot(questionnaire.data$STAIT, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with STAIT Score",
     ylab = "Valence Score",
     xlab = "STAIT Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$STAIT))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$STAIT)

###     FFMQ      ###

###Plot questionnaire.data against FFMQ_Ob###
plot(questionnaire.data$FFMQ_Ob, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with FFMQ_Ob Score",
     ylab = "Valence Score",
     xlab = "FFMQ_Ob Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Ob))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Ob)

###Plot questionnaire.data against FFMQ_Ds###
plot(questionnaire.data$FFMQ_Ds, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with FFMQ_Ds Score",
     ylab = "Valence Score",
     xlab = "FFMQ_Ds Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Ds))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Ds)

###Plot questionnaire.data against FFMQ_Aw###
plot(questionnaire.data$FFMQ_Aw, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with FFMQ_Aw Score",
     ylab = "Valence Score",
     xlab = "FFMQ_Aw Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Aw))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Aw)

###Plot questionnaire.data against FFMQ_Nj###
plot(questionnaire.data$FFMQ_Nj, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with FFMQ_Nj Score",
     ylab = "Valence Score",
     xlab = "FFMQ_Nj Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Nj))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Nj)

###Plot questionnaire.data against FFMQ_Nr###
plot(questionnaire.data$FFMQ_Nr, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with FFMQ_Nr Score",
     ylab = "Valence Score",
     xlab = "FFMQ_Nr Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Nr))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$FFMQ_Nr)

###     ERQ     ###

###Plot questionnaire.data against ERQ_CR###
plot(questionnaire.data$ERQ_CR, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with ERQ_CR Score",
     ylab = "Valence Score",
     xlab = "ERQ_CR Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$ERQ_CR))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$ERQ_CR)

###Plot questionnaire.data against ERQ_ES###
plot(questionnaire.data$ERQ_ES, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with ERQ_ES Score",
     ylab = "Valence Score",
     xlab = "ERQ_ES Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$ERQ_ES))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$ERQ_ES)

###     IUS     ###

###Plot questionnaire.data against IUS###
plot(questionnaire.data$IUS, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with IUS Score",
     ylab = "Valence Score",
     xlab = "IUS Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$IUS))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$IUS)


###     SWLS      ###

###Plot questionnaire.data against SWLS###
plot(questionnaire.data$SWLS, questionnaire.data$val_rate, 
     main="Val_Rate Correlation with SWLS Score",
     ylab = "Valence Score",
     xlab = "SWLS Score")
###do line of best fit###
abline(lm(questionnaire.data$val_rate ~ questionnaire.data$SWLS))
###Line characteristics, if desired###
lm(questionnaire.data$val_rate ~ questionnaire.data$SWLS)
dev.off()