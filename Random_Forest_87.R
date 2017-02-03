data<-read.csv("FINAL_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]

View(head(data))

set.seed(999)
#Splitting data into training and testing
train<-sample(1:32561,26049,replace = F)
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
                       importance=TRUE) 

importance(rf.fit)
varImpPlot(rf.fit)

# Confusion Matrix
preds <- predict(rf.fit, newdata=testing_data[,-14], type="response")
table(testing_data[,14], preds)
caret::confusionMatrix(testing_data[,-14], preds, mode = "prec_recall")
#87.11% Accuracy!!!