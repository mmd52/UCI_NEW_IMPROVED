#Author @ Mohammed 26/01/2017

#Load Libraries
source("Libraries.R")

#Running decision tree on normal Data 

data<-read.csv("FINALE_MOD_DATA_WITHOUT_NA.csv",header = T)
data<-data[,-c(1,6)]


set.seed(999)
boruta.train <- Boruta(income~., data = data, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)


boruta.df <- attStats(final.boruta)
print(boruta.df)
