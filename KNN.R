#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")

data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-1]
data<-data[,-5]

ndata<-data
View(head(ndata))
#======================================================================
#Preparing for KNN
#======================================================================
train_test<-ndata
features = names(train_test)
for (f in features) {
  if (class(train_test[[f]])=="factor") {
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
  }
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

set.seed(999)
#Splitting data into training and testing
train<-sample(1:32561,22793,replace = F)
test<--train

training_data<-train_test[train,]
testing_data<-train_test[test,]

knn_pred<-knn(training_data[,-14],testing_data[,-14],training_data[,14],9)

caret::confusionMatrix(testing_data[,14], knn_pred, mode = "prec_recall")

auc<-roc(testing_data[,14],abc)
print(auc)
plot(roc(testing_data$income,abc),print.auc=T)


#Accuracy is 75.91%
