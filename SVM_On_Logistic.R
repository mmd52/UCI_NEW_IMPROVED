#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")

data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]

View(head(data,50))
train_test<-data
features = names(train_test)

if (class(train_test[["income"]])=="factor") {
  levels <- unique(train_test[["income"]])
  train_test[["income"]] <- as.numeric(as.integer(factor(train_test[["income"]], levels=levels)))
}

for(i in 1:32561){
  if(train_test[i,14]==1)
  {
    train_test[i,14]=0
  }
  else if(train_test[i,14]==2)
  {
    train_test[i,14]=1
  }
}

data<-train_test
set.seed(999)
#Splitting data into training and testing
train<-sample(1:32561,22793,replace = F)
test<--train

training_data<-data[train,]
testing_data<-data[test,]

logit.fit=glm(income~.,family=binomial(logit),data=training_data)

summary(logit.fit)
#Great No multicolinearity here
vif(logit.fit)

preds<-ifelse(predict(logit.fit,newdata=testing_data[,-14],type="response")>=0.5,1,0)
table(testing_data[,14],preds)
caret::confusionMatrix(testing_data[,14], preds, mode = "prec_recall")

auc<-roc(testing_data[,14],ifelse(predict(logit.fit,newdata=testing_data[,-14],type="response")>=0.5,1,0))
print(auc)
plot(roc(testing_data[,14],ifelse(predict(logit.fit,newdata=testing_data[,-14],type="response")>=0.5,1,0)),print.auc=T)


#Accuracy is 85.37%


training_data$logit_f<-ifelse(predict(logit.fit,newdata=training_data[,-14],type="response")>=0.5,1,0)
testing_data$logit_f<-ifelse(predict(logit.fit,newdata=testing_data[,-14],type="response")>=0.5,1,0)
##======================SIMPLE SVM
model1.svm <- svm(income ~ ., data = training_data)
preds = ifelse(predict(model1.svm,newdata=testing_data[,-14],type="response")>=0.5,1,0)

caret::confusionMatrix(testing_data[,14], preds, mode = "prec_recall")
#==85.8% Accuracy

auc<-roc(as.numeric(testing_data[,14]),as.numeric(predict(model1.svm, testing_data[,-14])))
print(auc)
plot(auc,print.auc=T)
