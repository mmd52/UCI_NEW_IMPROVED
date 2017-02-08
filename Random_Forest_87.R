#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")

data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]

View(head(data))

set.seed(999)
#Splitting data into training and testing
train<-sample(1:32561,22793,replace = F)
test<--train

training_data<-data[train,]
testing_data<-data[test,]

#===================================================================
################################## Random Forest

# Tuning takes factors as target variables
bestmtry <- tuneRF(training_data[,-c(13,14)], as.factor(training_data[,14]), 
                   ntreeTry=100, stepFactor=1.5, improve=0.01,
                   trace=TRUE, plot=TRUE, dobest=FALSE) 


rf.fit <- randomForest(income ~ ., data=training_data[,-13], 
                       mtry=2, ntree=1000, keep.forest=TRUE, 
                       importance=TRUE,fold=10) 

varImpPlot(rf.fit)

# Confusion Matrix
preds <- predict(rf.fit, newdata=testing_data[,-c(13,14)], type="response")
table(testing_data[,14], preds)
labels<-as.factor(testing_data[,14])
caret::confusionMatrix(labels, preds, mode = "prec_recall")
#87.21% Accuracy!!!

auc<-roc(as.numeric(testing_data[,14]),as.numeric(predict(rf.fit, newdata=testing_data[,-c(13,14)], type="response")))
print(auc)
plot(auc,print.auc=T)
