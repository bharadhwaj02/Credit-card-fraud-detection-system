#Analysis Part

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

#Fitting The Logistic Regression Model
Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)

#predicting the accuracy
res<-predict(Logistic_Model,test_data,type = "response")
res
restr<-predict(Logistic_Model,train_data,type = "response")
restr

#Building the confusion matrix for training data
cm<-table(Actual_Value=train_data$Class,Predicted_Value=restr>0.5)
cm

#Accuracy
accuracy=(cm[[1,1]]+cm[[2,2]])/sum(cm)
accuracy

#Building  the confusion matrix for testing Data
cmte<-table(Actual_Value=test_data$Class,Predicted_Value=res>0.5)
cmte

#accuracy
accuracy=(cmte[[1,1]]+cmte[[2,2]])/sum(cmte)
accuracy

plot(Logistic_Model)

