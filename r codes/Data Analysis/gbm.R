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

#Gradient Boosting Classifier

library(gbm, quietly=TRUE)

# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 450
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)
# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")

model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
#Plot the gbm model

plot(model_gbm)

library(pROC)
# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Class, gbm_test, plot = TRUE, col = "Blue")
print(gbm_auc)

#predicting the accuracy
res<-predict(model_gbm,test_data,type = "response")
res
restr<-predict(model_gbm,train_data,type = "response")
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
