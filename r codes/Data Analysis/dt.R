#Get the workspace
getwd()
setwd("C:/Users/sruth/OneDrive/Desktop/CSE4027 LAB")
dir()
#reading the cleaned datset
data=read.csv("credit_cards.csv")
data

#data modelling
library(caTools)
set.seed(100)
das=sample.split(data$Class,SplitRatio=0.80)
train_data = subset(data,das==TRUE)
test_data = subset(data,das==FALSE)
dim(train_data)
dim(test_data)

#decision tree
library(rpart)
library(rpart.plot)
decisionrtee_model <- rpart(Class ~ . , data, method = 'class')
predicted_val <- predict(decisiontree_model, data, type = 'class')
probability <- predict(decisiontree_model, data, type = 'prob')
rpart.plot(decisiontree_model)

#building the confusion matrix 
tp<-predict(decisiontree_model,test_data,type = "class")
tp

tr<-predict(decisiontree_model,train_data,type = "class")
tr

#Building the confusion matrix for training data
cm<-table(Actual_Value=train_data$Class,Predicted_Value= tr)
cm

#Accuracy
accuracy=(cm[[1,1]]+cm[[2,2]])/sum(cm)
accuracy

#Building  the confusion matrix for testing Data
cmte<-table(Actual_Value=test_data$Class,Predicted_Value= tp)
cmte

#accuracy
accuracy=(cmte[[1,1]]+cmte[[2,2]])/sum(cmte)
accuracy
