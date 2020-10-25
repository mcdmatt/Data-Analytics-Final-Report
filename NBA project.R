library(arules)
library(arulesViz)
library(ggplot2)
library(kernlab)
library(dplyr)
library(jsonlite)
library(kernlab)
library(e1071)
'''
title: "SVM Analysis"
author: "Matt McDonnell"
date: "9/8/20
output: word_document
''' (r setup, include=F)
library(readr)
library(FactoMineR)
library(dplyr)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(knitr)


getwd()
NBAdata <- read.csv("/Users/mattmcdonnell/Downloads/dataset_20200829.csv")
dim(NBAdata)
 # Support Vector Machine Analysis
library(caret)
NBAdata <- NBAdata[sample(nrow(NBAdata), 1000), ]

# SVMA ______Predicting a win______________________________________________________________________________________________
svmWin <- data.frame(Location = NBAdata$LOCATION, Win = NBAdata$W, 
                  Opponent = NBAdata$opponent)

#create the train (2/3) and test (1/3) sets
trainList <- createDataPartition(y= svmWin$Win, p=.7, list=FALSE)
trainData <- svmWin[trainList,]
testData <- svmWin[-trainList,]

#Run the model
svmOutput <- ksvm(Win ~., data=trainData, kernel="rbfdot", kpar="automatic",
                  C=30, cross=3, prob.model=TRUE) # Usinig a larger C to 
svmOutput
hist(alpha(svmOutput)[[1]],
     main = "Support Vector with C=30", 
     xlab = "Support Vector Values")
# The many numbers  to the right side of the histogram mean it's  hard for the model to predict the zone properly
# The numbers on the left had side of the histogram are too simple and easy to predict so don't offer any use in modeling
# I alter this with a different 'C=  '
svmPred <- predict(svmOutput, testData)
svmPred
confusionMatrix(testData$Win, svmPred)

# Visualizing a .5 Threshold of the best predictive svm analysis
tp <- c(0, 0.6471, 1 ) # The Sensitivity of the model acts as the true positive rate
1-0.6167 # 0.3833
fp <- c(0 ,0.3833, 1) # The Specificity shows the false negative rate so subtracting by 1 gives is the false positive rate
plot(fp,tp, pch = 19, col = "red", xlab = "False Positive Rate", 
     ylab =  "True Positive Rate", main = )
lines(fp,tp, col = "red")
xadj <- c(.02, 0, -.04)
yadj <- c(0.02,.03, -.05)
text(x = fp + xadj, y = tp + yadj, 
     labels = c("Threshold\n0", "Threshold\n.5", "Threshold\n1"))
abline(v = 0, lty = 2)
abline(h = 1, lty = 2)
text(.13, .96, labels = "Ideal Model")
points(0,1, pch = "O", cex = 1.5)


str(NBAdata)
# SVMA ______Predicting a successful shot attempt________________________________________________________________________
svmScore <- data.frame(SHOT_RESULT = NBAdata$SHOT_RESULT, PTS = as.factor(NBAdata$PTS), 
                  PTS_TYPE = as.factor(NBAdata$PTS_TYPE), SHOT_NUMBER = as.factor(NBAdata$SHOT_NUMBER), DRIBBLES = as.factor(NBAdata$DRIBBLES), 
                  TOUCH_TIME = as.factor(NBAdata$TOUCH_TIME),locationX = as.factor(NBAdata$locationX), locationY = as.factor(NBAdata$locationY), 
                  SHOT_DIST = as.factor(NBAdata$SHOT_DIST),zoneBasic = as.factor(NBAdata$zoneBasic), PERIOD = as.factor(NBAdata$PERIOD), 
                  shotclock = as.factor(NBAdata$shotclock),CLOSE_DEF_DIST = as.factor(NBAdata$CLOSE_DEF_DIST), zoneRange = as.factor(NBAdata$zoneRange), 
                  nameZone = as.factor(NBAdata$nameZone), gameclock = NBAdata$gameclock)

#create the train abour (2/3) and test (1/3) sets
trainList2 <- createDataPartition(y= svmScore$SHOT_RESULT, p=.7, list=FALSE)
trainData2 <- svmScore[trainList2,]
testData2 <- svmScore[-trainList2,]

#Run the model
svmOutput2 <- ksvm(SHOT_RESULT ~., data=trainData2, kernel="tanhdot", kpar="automatic",
                  C=1000, cross=3, prob.model=TRUE) # Usinig a larger C to 
svmOutput2
hist(alpha(svmOutput2)[[1]],
     main = "Support Vector with C=1000", 
     xlab = "Support Vector Values")

# The many numbers  to the right side of the histogram mean it's  hard for the model to predict the zone properly
# The numbers on the left had side of the histogram are too simple and easy to predict so don't offer any use in modeling
# I alter this with a different 'C=  '
# Ideally it would be more balanced but R shuts down when increasing C too much
# C=100 is a good compromise
svmPred2 <- predict(svmOutput2, testData2)
svmPred2
confusionMatrix(testData2$SHOT_RESULT, svmPred2)


# Visualizing a .5 Threshold of the best predictive svm analysis
tp <- c(0, 0.7778, 1 ) # The Sensitivity of the model acts as the true positive rate
1-0.8092 # 0.1908
fp <- c(0 ,0.1908, 1) # The Specificity shows the false negative rate so subtracting by 1 gives is the false positive rate
plot(fp,tp, pch = 19, col = "red", xlab = "False Positive Rate", 
     ylab =  "True Positive Rate", main = )
lines(fp,tp, col = "red")
xadj <- c(.02, 0, -.04)
yadj <- c(0.02,.03, -.05)
text(x = fp + xadj, y = tp + yadj, 
     labels = c("Threshold\n0", "Threshold\n.5", "Threshold\n1"))
abline(v = 0, lty = 2)
abline(h = 1, lty = 2)
text(.1, .97, labels = "Ideal Model")
points(0,1, pch = "O", cex = 1.5)





