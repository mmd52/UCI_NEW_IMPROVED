data<-read.csv("despo.csv",header = T)
ndata<-data
table(as.factor(data$type_employer))

#Here we have 1836 Missing values
View(head(data,70))
training_data<-data[!is.na(data$type_employer),-c(15)]
testing_data<-data[is.na(data$type_employer),-c(2,15)]
#======================================================================
################################## Random Forest
#===================================================================
bestmtry <- tuneRF(training_data[,-2], as.factor(training_data[,2]), 
                   ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE,
                   plot=TRUE, dobest=FALSE) 




rf.fit <- randomForest(type_employer ~ ., data=training_data, 
                       mtry=2, ntree=1000, keep.forest=TRUE,
                       importance=TRUE) 



importance(rf.fit)
varImpPlot(rf.fit)

# Confusion Matrix
preds <- predict(rf.fit, newdata=testing_data, type="response")

ndata<-data
View(ndata)
ndata$type_employer[is.na(ndata$type_employer)]<-
  predict(rf.fit,ndata[is.na(ndata$type_employer),-c(15)],
          type="response")

table(as.factor(ndata$type_employer))

write.csv(ndata,"despo2.csv")
#======================================================================
rm(data,ndata,bestmtry,testing_data,training_data,preds)
data<-read.csv("despo2.csv",header = T)
table(as.factor(data$occupation))
summary(data$occupation)
#Here we have 1843 Missing values
View(head(data,70))
training_data<-data[!is.na(data$occupation),-c(15)]
testing_data<-data[is.na(data$occupation),-c(7,15)]
#======================================================================
################################## Random Forest
#===================================================================
bestmtry <- tuneRF(training_data[,-c(7,14)], 
                   as.factor(training_data[,7]), 
                   ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE,
                   plot=TRUE, dobest=FALSE,na.action=na.omit) 




rf.fit <- randomForest(occupation ~ ., data=training_data[,-14], 
                       mtry=2, ntree=1000, keep.forest=TRUE,
                       importance=TRUE) 



importance(rf.fit)
varImpPlot(rf.fit)

ndata<-data
ndata$occupation[is.na(ndata$occupation)]<-
  predict(rf.fit,ndata[is.na(ndata$occupation),-c(14,15)],
          type="response")

table(as.factor(ndata$type_employer))

write.csv(ndata,"despo3.csv")

rm(bestmtry,data,ndata,testing_data,training_data)
#====================================================================
data<-read.csv("despo3.csv",header = T)
View(data)
data<-data[,-1]
table(as.factor(data$country))
summary(data$country)
#Here we have 583 Missing values
training_data<-data[!is.na(data$country),-c(15)]
testing_data<-data[is.na(data$country),-c(14,15)]
#======================================================================
################################## Random Forest
#===================================================================
bestmtry <- tuneRF(training_data[,-14], as.factor(training_data[,14]), 
                   ntreeTry=100, stepFactor=1.5, improve=0.01, trace=TRUE,
                   plot=TRUE, dobest=FALSE) 


rf.fit <- randomForest(country ~ ., data=training_data, 
                       mtry=2, ntree=1000, keep.forest=TRUE,
                       importance=TRUE) 



importance(rf.fit)
varImpPlot(rf.fit)

ndata<-data
View(ndata)

ndata$country[is.na(ndata$country)]<-
  predict(rf.fit,ndata[is.na(ndata$country),-c(15)],
          type="response")

summary(ndata$country)

write.csv(ndata,"FINALE_MOD_DATA_WITHOUT_NA.csv")
#=========================================
#=============================