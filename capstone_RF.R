rm(list=ls(all=TRUE))

# This is an already cleaned dataset.
application_train <- read.csv('Capstone/application_train_clean.csv',1)
dim(application_train)

names(application_train)

# convert categorical attributes to factor type
data_cat <- application_train[,c(1:49)]
data_num <- application_train[,-c(1:49)]
data_cat <- data.frame(apply(data_cat, 2,function(x){as.factor(x)}))
data_num <- data.frame(apply(data_num, 2,function(x){as.numeric(x)}))
application_train <- cbind(data_cat,data_num)
str(application_train)

# class distribution
table(application_train$TARGET)


# Tried the model first without SMOTE

set.seed(7)
### now using SMOTE to create a more "balanced problem"
library(DMwR)
application_train <- SMOTE(TARGET ~ ., application_train, perc.over = 200, perc.under = 200)
table(application_train$TARGET)

# Split Data into Train and test - 70:30
library(caret)
set.seed(7)
train_test_split<-createDataPartition(application_train$TARGET, p=0.7, list = FALSE)
train_data<-application_train[train_test_split,]
test_data<-application_train[-train_test_split,]

# train - class distribution
table(train_data$TARGET)
# test - class distribution
table(test_data$TARGET)

library(randomForest)
set.seed(7)
rf <- randomForest(TARGET ~ ., data=train_data[,-c(1)], keep.forest=TRUE, mtry=18, importance=TRUE) 
print(rf)

# Plotting OOB error rates
plot(rf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Train data")

print(rf$importance)
# Evaluate variable importance
importance(rf)
# plot (directly prints the important attributes) 
varImpPlot(rf,
           sort = T,
           main="Variable Importance",
           n.var=30)
# Variable Importance Table
var.imp <- data.frame(importance(rf,type=2))

# Important Features
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
imp_feature<-var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]$Variables

# Find the optimal mtry
mtry <- tuneRF(train_data[,-c(1)],train_data$TARGET, ntreeTry=30,
stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Prediction and Calculate Performance Metrics
predictions <- predict(rf,train_data[,-c(1)],type='response')
library(pROC)
roc.multi <- multiclass.roc(train_data$TARGET,as.numeric(predictions))
auc(roc.multi)
rs<-roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

# Predict on Train data 
pred_model_train <-predict(rf,train_data[,-c(1,2)],type="response")
result_train <- table("actual _values"= train_data$TARGET,pred_model_train);result_train
# Accuracy,Precision and Recall on train
train_accuracy <- sum(diag(result_train))/sum(result_train)*100;train_accuracy

pred_model_train_prob <-predict(rf,train_data[,-c(1,2)],type="prob")

# scoring
train_data$predict.class <- predict(rf, train_data[,-c(1)], type="class")
train_data$predict.score <- predict(rf, train_data[,-c(1)], type="prob")
head(train_data)

# Predict on test data 
pred_model_test <-predict(rf,test_data[,-c(1,2)],type="response")
result_test <- table("actual _values"= test_data$TARGET,pred_model_test);result_test
# Accuracy,Precision and Recall on train
test_accuracy <- sum(diag(result_test))/sum(result_test)*100;test_accuracy

pred_model_test_prob <-predict(rf,test_data[,-c(1,2)],type="prob")

# scoring
test_data$predict.class <- predict(rf, test_data[,-c(1)], type="class")
test_data$predict.score <- predict(rf, test_data[,-c(1)], type="prob")
head(test_data)

# Prediction and Calculate Performance Metrics
predictions <- predict(rf,test_data[,-c(1)],type='response')
library(pROC)
roc.multi <- multiclass.roc(test_data$TARGET,as.numeric(predictions))
auc(roc.multi)
rs<-roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))






