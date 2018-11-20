setwd("E:/Analytics Course/Capstone Project")

## Reading training data

train_data<-read.csv("application_train.csv")
attach(train_data)

dim(train_data)
str(train_data)

head(train_data)

sum(is.na(train_data))

## Removing columns not required for clustering

new_train_data<-train_data[,-c(1,25:26,35:42,61:93,98:118)]
dim(new_train_data)

## identifying missing values

sum(is.na(new_train_data))
summary(new_train_data)

str(new_train_data)
library(DMwR)
levels(new_train_data$DAYS_BIRTH)
attach(new_train_data)

## Converting variables to factors

new_train_data$TARGET<-as.factor(new_train_data$TARGET)
new_train_data$FLAG_MOBIL<-as.factor(new_train_data$FLAG_MOBIL)
new_train_data$FLAG_EMAIL<-as.factor(new_train_data$FLAG_EMAIL)
new_train_data$FLAG_EMP_PHONE<-as.factor(new_train_data$FLAG_EMP_PHONE)
new_train_data$FLAG_PHONE<-as.factor(new_train_data$FLAG_PHONE)
new_train_data$REGION_RATING_CLIENT<-as.factor(new_train_data$REGION_RATING_CLIENT)
new_train_data$REGION_RATING_CLIENT_W_CITY<-as.factor(new_train_data$REGION_RATING_CLIENT_W_CITY)

## Impute Missing values

library(mlr)

train_impute<-impute(new_train_data,target = "TARGET",classes = list(numeric = imputeMedian(),factor = imputeMode()))
new_train_data<-train_impute$data

colnames(new_train_data)

sum(is.na(new_train_data))
levels(TARGET)
levels(new_train_data$TARGET)

## Spliting data

library(plyr)
count(new_train_data,vars = "TARGET")
dim(new_train_data)

set.seed(123)
trainrow<- new_train_data[sample(1:nrow(new_train_data),5000),]
head(trainrow)

count(trainrow,vars = "TARGET")

## Dropping column with target variable

trainrow<-trainrow[,-1]
colnames(trainrow)
sum(is.na(trainrow))

## As data contains both numeric and categorical variables, kmeans will not be applicable

## We will use Gower distance to caluclate distance between the 

str(trainrow)
dim(trainrow)

install.packages("cluster")
library(cluster)
?daisy

gowerdist<-daisy(trainrow,metric = "gower",type = list(logratio = c(1:56)))
summary(gowerdist)

## developing matrix to calculate distance between 2 most similar and 2 most dissimilar

gower_mat<-as.matrix(gowerdist)

## calculating sil width

sil_width<-c(NA)

for (i in 2:10) {pam_fit<-pam(gowerdist,diss = TRUE, k = i) 

sil_width[i]<-pam_fit$silinfo$avg.width
  
}

## Plotting Silhouette width

plot(1:10,sil_width)

## NUmber of clusters can be 3 or 4

pam_fit<-pam(gowerdist,diss = TRUE, k = 3)

mutate(trainrow,cluster = pam_fit$clustering)

library(dplyr)

pam_results <- trainrow %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


