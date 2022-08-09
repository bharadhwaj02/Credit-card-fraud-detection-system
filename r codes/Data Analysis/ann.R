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


#Fitting ANN model

library(neuralnet)
ANN_model =neuralnet(Class~.,train_data,linear.output=FALSE)
plot(ANN_model)

predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)

Antp<-predict(ANN_model ,test_data,type = "class")
Antp

cm<-table(Actual_Value=test_data$Class,Predicted_Value= Antp)
cm

#Accuracy
accuracy=(cm[[1,1]]+cm[[2,2]])/sum(cm)
accuracy

#Building  the confusion matrix for training Data
Antr<-predict(ANN_model ,train_data,type = "class")
cmte<-table(Actual_Value=train_data$Class,Predicted_Value= Antr)
cmte

#accuracy
accuracy=(cmte[[1,1]]+cmte[[2,2]])/sum(cmte)
accuracy
