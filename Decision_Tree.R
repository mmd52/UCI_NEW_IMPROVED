#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")

#Running decision tree on normal Data 

data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]

set.seed(999)
#Splitting data into training and testing
train<-sample(1:32561,22793,replace = F)
test<--train

training_data<-data[train,]
testing_data<-data[test,]

#To run a decision tree and make a model im going to make use of 
#package RWEKA here

j48<-J48(income~.,data=training_data,control=Weka_control(),options=NULL)

summary(j48)

eval_j48 <- evaluate_Weka_classifier(j48, numFolds = 100,
                                     complexity = FALSE, 
                                     seed = 1, class = TRUE)
eval_j48

preds<-predict(j48,testing_data[,-14])
caret::confusionMatrix(testing_data[,14], preds, mode = "prec_recall")