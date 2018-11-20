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

summary(application_train)

# Notice that the following categorical features have all its data in just 1 class. Let's drop them from the model.
# FLAG_DOCUMENT_12
# 0:307509
# 1: 2
# FLAG_DOCUMENT_10
# 0:307504
# 1: 7
# FLAG_MOBIL
# 0: 1
# 1:307510

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

# Logistic Regression Model - all features
logit_model1<-glm(TARGET~., data = train_data[,-c(1,12,38,40)], family = binomial(link = 'logit'))
summary(logit_model1)

library(lmtest)
# Log Likelihood Test
lrtest(logit_model1)

library(pscl)
# McFadden Pseudo RSquare Test
pR2(logit_model1)

# Odds Ratio
odd_model<-exp(coef(logit_model1))
odd_model

# Predict the outcome - train
predict_prob<-predict(logit_model1,train_data[,-c(1,2)], type="response")
predicted_response<-ifelse(predict_prob>0.5,1,0) 
predicted_response<-as.factor(predicted_response)
##Confusion Matrix
confusionMatrix(predicted_response,train_data$TARGET)

library(ROCR)
# ROC - training data
pred<-predict(logit_model1,train_data[,-c(1,2)], type='response')
pred<-prediction(pred, train_data$TARGET)
roc<-performance(pred,"tpr", "fpr")
plot(roc)
auc<-performance(pred,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

# Predict the outcome - test
predict_prob<-predict(logit_model1,test_data[,-c(1,2)], type="response")
predicted_response<-ifelse(predict_prob>0.5,1,0) 
predicted_response<-as.factor(predicted_response)
##Confusion Matrix
confusionMatrix(predicted_response,test_data$TARGET)

# ROC - test data
pred<-predict(logit_model1,test_data[,-c(1,2)], type='response')
pred<-prediction(pred, test_data$TARGET)
roc<-performance(pred,"tpr", "fpr")
plot(roc)
auc<-performance(pred,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

# Feature Selection - 1
library(car)
#** Multi-collinearity check
vif(logit_model1)

# Feature Selection - 2
#** run anova
anova(logit_model1, test = 'Chisq')

# Feature Selection - 3
#** Identify Redundant Features
set.seed(7)
library(mlbench)
# calculate correlation matrix
correlationMatrix <- cor(application_train[,-c(1:49)])
# summarize the correlation matrix
print(correlationMatrix)
library(corrplot)
opar2 <- par(no.readonly = TRUE)
corrplot(correlationMatrix,method = "circle",tl.cex = 0.5,tl.col = "black",number.cex = 0.55,bg = "grey14",
         addgrid.col = "gray50", tl.offset = 2,col = colorRampPalette(c("blue1","ivory2","firebrick2"))(100))
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Feature Selection - 4
#** Features ranked by Importance
set.seed(7)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(TARGET~., data=train_data[,-c(1)], method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# Feature Selection - 5
#** Recursive Feature Elimination
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train_data[,-c(1,2)], cellphone_train_data[,c(2)], sizes=c(3:74), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

