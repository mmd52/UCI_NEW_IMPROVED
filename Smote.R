#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")
library(DMwR)

data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]

summary(data$income)

train_test<-data
features = names(train_test)
for (f in features) {
  if (class(train_test[[f]])=="factor") {
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
  }
}

for(i in 1:22048){
  if(train[i,14]==1)
  {
    train[i,14]=0
  }
  else if(train[i,14]==2)
  {
    train[i,14]=1
  }
}
View(head(train_test))

summary(as.factor(train_test$income))

set.seed(999)
splitIndex<-createDataPartition(train_test$income,p=.70,list=F,times=1)
train<-train_test[splitIndex,]
test<-train_test[-splitIndex,]

summary(as.factor(train$income))


dtrain = xgb.DMatrix(as.matrix(train[,-14]), 
                     label=train[,14])
dtest = xgb.DMatrix(as.matrix(test[,-14]))

xgb_param_adult = list(
  nrounds = c(700),
  eta = 0.075,#eta between(0.01-0.2)
  max_depth = 6, #values between(3-10)
  subsample = 0.7,#values between(0.5-1)
  colsample_bytree = 0.7,#values between(0.5-1)
  num_parallel_tree=1,
  objective='binary:logistic',
  #booster='gbtree',
  min_child_weight = 1
  #eval_metric="auc"
)

res = xgb.cv(xgb_param_adult,
             dtrain,
             nrounds=200,   # changed
             nfold=10,           # changed
             early_stopping_rounds=50,
             print_every_n = 10,
             verbose= 1)

xgb.fit = xgb.train(xgb_param_adult, dtrain, 200)

auc<-roc(test$income,predict(xgb.fit,dtest))
print(auc)

train$income<-as.factor(train$income)
summary(as.factor(train$income))
cpy<-train
train<-cpy
train<-SMOTE(income~.,train,perc.over=100,k = 9)
train$income<-as.numeric(train$income)

# Confusion Matrix
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(test[,-14])) >= 0.5, 1, 0)
caret::confusionMatrix(test[,14], preds, mode = "prec_recall")