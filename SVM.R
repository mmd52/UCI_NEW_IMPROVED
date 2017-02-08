#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")

#SVM
data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]
View(head(data))

set.seed(999)
#Splitting data into training and testing
train<-sample(1:32561,22793,replace = F)
test<--train

training_data<-data[train,]
testing_data<-data[test,]

##======================SIMPLE SVM
model1.svm <- svm(income ~ ., data = training_data)
preds = predict(model1.svm, testing_data[,-14])
caret::confusionMatrix(testing_data[,14], preds, mode = "prec_recall")
#==85.8% Accuracy

auc<-roc(as.numeric(testing_data[,14]),as.numeric(predict(model1.svm, testing_data[,-14])))
print(auc)
plot(auc,print.auc=T)
