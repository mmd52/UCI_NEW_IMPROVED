#Author @ Mohammed 26/01/2017
#Code used to Fetch/install/Load all libraries required

print("============== Beginning to load libraries  ===================")

#Function To check if 
checkInstallLoad <- function(libName) 
{
  if(!require(libName, character.only=TRUE)) 
  {
    install.packages(libName)
    require(libName, character.only=TRUE)
  }
}


#Calling the libraries for Decision tree
checkInstallLoad("RWeka")
checkInstallLoad("partykit")

#calling the libraries for Logistic Regression
checkInstallLoad("car")

#calling the libraries for Naive Bayes
checkInstallLoad("e1071")

#calling the libraries for Random forest
checkInstallLoad("class")
checkInstallLoad("data.table")
checkInstallLoad("Matrix")
checkInstallLoad("YaleToolkit")
checkInstallLoad("Amelia")
checkInstallLoad("Metrics")
checkInstallLoad("plyr")
checkInstallLoad("dplyr")
checkInstallLoad("stringr")
checkInstallLoad("lubridate")
checkInstallLoad("ggplot2")
checkInstallLoad("plot3D")
checkInstallLoad("pROC")
checkInstallLoad("caret")
checkInstallLoad("caretEnsemble")
checkInstallLoad("e1071")
checkInstallLoad("randomForest")
checkInstallLoad("xgboost")
checkInstallLoad("rpart")
checkInstallLoad("C50")
checkInstallLoad("adabag")
checkInstallLoad("arules")
checkInstallLoad("ROCR")
checkInstallLoad("nnet")
checkInstallLoad("car")
checkInstallLoad("Ckmeans.1d.dp")

#For Boruta
checkInstallLoad("Boruta")

#For Smote
checkInstallLoad("DMwR")

print("============== Completed loading libraries  ===================")
