rm(list=ls(all=TRUE))

application_train <- read.csv('Capstone//application_train.csv',1)
dim(application_train)

str(application_train)

# convert categorical attributes to factor type
data_cat <- application_train[,c(1:6,12:16,23:29,31:41,87,88,90,91,97:116)]
data_num <- application_train[,-c(1:6,12:16,23:29,31:41,87,88,90,91,97:116)]

data_cat <- data.frame(apply(data_cat, 2,function(x){as.factor(x)}))
data_num <- data.frame(apply(data_num, 2,function(x){as.numeric(x)}))
application_train <- cbind(data_cat,data_num)

str(application_train)

### continuous attributes
names(application_train)[which(sapply(application_train, is.numeric))]
### categorical attributes
names(application_train)[which(sapply(application_train,is.factor))]

### Check for constants
sapply(application_train,function(x) length(unique(x)))

table(is.na(application_train))
# there are a lot of missing values

### Check for null or missing values
cbind(sort(colSums(is.na(application_train)),decreasing=TRUE))
### Check for null or missing values percentage
sort(sapply(application_train, function(x) sum(is.na(x))/length(x))*100,decreasing=TRUE)

# Five-point summary
summary(application_train)

# Outlier treatment will be performed prior to missing value treatment.

summary(application_train$CNT_CHILDREN)
# Check for outliers - Univariate approach
boxplot(application_train$CNT_CHILDREN, main="CNT_CHILDREN", boxwex=0.1)
#boxplot.stats(application_train$CNT_CHILDREN)$out
# Check for outliers - Bivariate approach
# boxplot(CNT_CHILDREN ~ CNT_FAM_MEMBERS, data=application_train, 
#        main="Boxplot for Family Size vs Children count")

# We see most children counts as outliers, however, we will cap them 
# between 1 percentile and 99 percentile as an exception.

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$CNT_CHILDREN
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$CNT_CHILDREN[application_train$CNT_CHILDREN < (qnt[1] - H)] <- caps[1]
application_train$CNT_CHILDREN[application_train$CNT_CHILDREN > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_INCOME_TOTAL)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_INCOME_TOTAL, main="AMT_INCOME_TOTAL", boxwex=0.1)
#boxplot.stats(application_train$AMT_INCOME_TOTAL)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_INCOME_TOTAL
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_INCOME_TOTAL[application_train$AMT_INCOME_TOTAL < (qnt[1] - H)] <- caps[1]
application_train$AMT_INCOME_TOTAL[application_train$AMT_INCOME_TOTAL > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_CREDIT)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_CREDIT, main="AMT_CREDIT", boxwex=0.1)
#boxplot.stats(application_train$AMT_CREDIT)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_CREDIT
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_CREDIT[application_train$AMT_CREDIT < (qnt[1] - H)] <- caps[1]
application_train$AMT_CREDIT[application_train$AMT_CREDIT > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_ANNUITY)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_ANNUITY, main="AMT_ANNUITY", boxwex=0.1)
#boxplot.stats(application_train$AMT_ANNUITY)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_ANNUITY
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_ANNUITY[application_train$AMT_ANNUITY < (qnt[1] - H)] <- caps[1]
application_train$AMT_ANNUITY[application_train$AMT_ANNUITY > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_GOODS_PRICE)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_GOODS_PRICE, main="AMT_GOODS_PRICE", boxwex=0.1)
#boxplot.stats(application_train$AMT_GOODS_PRICE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_GOODS_PRICE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_GOODS_PRICE[application_train$AMT_GOODS_PRICE < (qnt[1] - H)] <- caps[1]
application_train$AMT_GOODS_PRICE[application_train$AMT_GOODS_PRICE > (qnt[2] + H)] <- caps[2]

summary(application_train$REGION_POPULATION_RELATIVE)
# Check for outliers - Univariate approach
boxplot(application_train$REGION_POPULATION_RELATIVE, main="REGION_POPULATION_RELATIVE", boxwex=0.1)
#boxplot.stats(application_train$REGION_POPULATION_RELATIVE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$REGION_POPULATION_RELATIVE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$REGION_POPULATION_RELATIVE[application_train$REGION_POPULATION_RELATIVE < (qnt[1] - H)] <- caps[1]
application_train$REGION_POPULATION_RELATIVE[application_train$REGION_POPULATION_RELATIVE > (qnt[2] + H)] <- caps[2]

summary(application_train$DAYS_EMPLOYED)
# Check for outliers - Univariate approach
boxplot(application_train$DAYS_EMPLOYED, main="DAYS_EMPLOYED", boxwex=0.1)
#boxplot.stats(application_train$DAYS_EMPLOYED)$out

# There is an invalid value = 365243 days ~ 1000 years. We can mark those as NA first 
# before proceeding further.

application_train$DAYS_EMPLOYED[application_train$DAYS_EMPLOYED == 365243] <- NA

summary(application_train$DAYS_EMPLOYED)
# Check for outliers - Univariate approach
boxplot(application_train$DAYS_EMPLOYED, main="DAYS_EMPLOYED", boxwex=0.1)
#boxplot.stats(application_train$DAYS_EMPLOYED)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$DAYS_EMPLOYED
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) # changed the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$DAYS_EMPLOYED[application_train$DAYS_EMPLOYED < (qnt[1] - H)] <- caps[1]
application_train$DAYS_EMPLOYED[application_train$DAYS_EMPLOYED > (qnt[2] + H)] <- caps[2]

summary(application_train$DAYS_REGISTRATION)
# Check for outliers - Univariate approach
boxplot(application_train$DAYS_REGISTRATION, main="DAYS_REGISTRATION", boxwex=0.1)
#boxplot.stats(application_train$DAYS_REGISTRATION)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$DAYS_REGISTRATION
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$DAYS_REGISTRATION[application_train$DAYS_REGISTRATION < (qnt[1] - H)] <- caps[1]
application_train$DAYS_REGISTRATION[application_train$DAYS_REGISTRATION > (qnt[2] + H)] <- caps[2]

summary(application_train$OWN_CAR_AGE)
# Check for outliers - Univariate approach
boxplot(application_train$OWN_CAR_AGE, main="OWN_CAR_AGE", boxwex=0.1)
#boxplot.stats(application_train$OWN_CAR_AGE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$OWN_CAR_AGE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$OWN_CAR_AGE[application_train$OWN_CAR_AGE < (qnt[1] - H)] <- caps[1]
application_train$OWN_CAR_AGE[application_train$OWN_CAR_AGE > (qnt[2] + H)] <- caps[2]

summary(application_train$CNT_FAM_MEMBERS)
# Check for outliers - Univariate approach
boxplot(application_train$CNT_FAM_MEMBERS, main="CNT_FAM_MEMBERS", boxwex=0.1)
#boxplot.stats(application_train$CNT_FAM_MEMBERS)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$CNT_FAM_MEMBERS
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) # changed the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$CNT_FAM_MEMBERS[application_train$CNT_FAM_MEMBERS < (qnt[1] - H)] <- caps[1]
application_train$CNT_FAM_MEMBERS[application_train$CNT_FAM_MEMBERS > (qnt[2] + H)] <- caps[2]

summary(application_train$APARTMENTS_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$APARTMENTS_AVG, main="APARTMENTS_AVG", boxwex=0.1)
#boxplot.stats(application_train$APARTMENTS_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$APARTMENTS_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$APARTMENTS_AVG[application_train$APARTMENTS_AVG < (qnt[1] - H)] <- caps[1]
application_train$APARTMENTS_AVG[application_train$APARTMENTS_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$BASEMENTAREA_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$BASEMENTAREA_AVG, main="BASEMENTAREA_AVG", boxwex=0.1)
#boxplot.stats(application_train$BASEMENTAREA_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$BASEMENTAREA_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$BASEMENTAREA_AVG[application_train$BASEMENTAREA_AVG < (qnt[1] - H)] <- caps[1]
application_train$BASEMENTAREA_AVG[application_train$BASEMENTAREA_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$YEARS_BEGINEXPLUATATION_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$YEARS_BEGINEXPLUATATION_AVG, main="YEARS_BEGINEXPLUATATION_AVG", boxwex=0.1)
#boxplot.stats(application_train$YEARS_BEGINEXPLUATATION_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$YEARS_BEGINEXPLUATATION_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$YEARS_BEGINEXPLUATATION_AVG[application_train$YEARS_BEGINEXPLUATATION_AVG < (qnt[1] - H)] <- caps[1]
application_train$YEARS_BEGINEXPLUATATION_AVG[application_train$YEARS_BEGINEXPLUATATION_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$YEARS_BUILD_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$YEARS_BUILD_AVG, main="YEARS_BUILD_AVG", boxwex=0.1)
#boxplot.stats(application_train$YEARS_BUILD_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$YEARS_BUILD_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$YEARS_BUILD_AVG[application_train$YEARS_BUILD_AVG < (qnt[1] - H)] <- caps[1]
application_train$YEARS_BUILD_AVG[application_train$YEARS_BUILD_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$COMMONAREA_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$COMMONAREA_AVG, main="COMMONAREA_AVG", boxwex=0.1)
#boxplot.stats(application_train$COMMONAREA_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$COMMONAREA_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$COMMONAREA_AVG[application_train$COMMONAREA_AVG < (qnt[1] - H)] <- caps[1]
application_train$COMMONAREA_AVG[application_train$COMMONAREA_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$ELEVATORS_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$ELEVATORS_AVG, main="ELEVATORS_AVG", boxwex=0.1)
#boxplot.stats(application_train$ELEVATORS_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$ELEVATORS_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$ELEVATORS_AVG[application_train$ELEVATORS_AVG < (qnt[1] - H)] <- caps[1]
application_train$ELEVATORS_AVG[application_train$ELEVATORS_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$ENTRANCES_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$ENTRANCES_AVG, main="ENTRANCES_AVG", boxwex=0.1)
#boxplot.stats(application_train$ENTRANCES_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$ENTRANCES_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$ENTRANCES_AVG[application_train$ENTRANCES_AVG < (qnt[1] - H)] <- caps[1]
application_train$ENTRANCES_AVG[application_train$ENTRANCES_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$FLOORSMAX_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$FLOORSMAX_AVG, main="FLOORSMAX_AVG", boxwex=0.1)
#boxplot.stats(application_train$FLOORSMAX_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$FLOORSMAX_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$FLOORSMAX_AVG[application_train$FLOORSMAX_AVG < (qnt[1] - H)] <- caps[1]
application_train$FLOORSMAX_AVG[application_train$FLOORSMAX_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$FLOORSMIN_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$FLOORSMIN_AVG, main="FLOORSMIN_AVG", boxwex=0.1)
#boxplot.stats(application_train$FLOORSMIN_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$FLOORSMIN_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$FLOORSMIN_AVG[application_train$FLOORSMIN_AVG < (qnt[1] - H)] <- caps[1]
application_train$FLOORSMIN_AVG[application_train$FLOORSMIN_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$LANDAREA_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$LANDAREA_AVG, main="LANDAREA_AVG", boxwex=0.1)
#boxplot.stats(application_train$LANDAREA_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LANDAREA_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LANDAREA_AVG[application_train$LANDAREA_AVG < (qnt[1] - H)] <- caps[1]
application_train$LANDAREA_AVG[application_train$LANDAREA_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$LIVINGAPARTMENTS_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$LIVINGAPARTMENTS_AVG, main="LIVINGAPARTMENTS_AVG", boxwex=0.1)
#boxplot.stats(application_train$LIVINGAPARTMENTS_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LIVINGAPARTMENTS_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LIVINGAPARTMENTS_AVG[application_train$LIVINGAPARTMENTS_AVG < (qnt[1] - H)] <- caps[1]
application_train$LIVINGAPARTMENTS_AVG[application_train$LIVINGAPARTMENTS_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$LIVINGAREA_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$LIVINGAREA_AVG, main="LIVINGAREA_AVG", boxwex=0.1)
#boxplot.stats(application_train$LIVINGAREA_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LIVINGAREA_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LIVINGAREA_AVG[application_train$LIVINGAREA_AVG < (qnt[1] - H)] <- caps[1]
application_train$LIVINGAREA_AVG[application_train$LIVINGAREA_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$NONLIVINGAPARTMENTS_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$NONLIVINGAPARTMENTS_AVG, main="NONLIVINGAPARTMENTS_AVG", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAPARTMENTS_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$NONLIVINGAPARTMENTS_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$NONLIVINGAPARTMENTS_AVG[application_train$NONLIVINGAPARTMENTS_AVG < (qnt[1] - H)] <- caps[1]
application_train$NONLIVINGAPARTMENTS_AVG[application_train$NONLIVINGAPARTMENTS_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$NONLIVINGAREA_AVG)
# Check for outliers - Univariate approach
boxplot(application_train$NONLIVINGAREA_AVG, main="NONLIVINGAREA_AVG", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAREA_AVG)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$NONLIVINGAREA_AVG
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$NONLIVINGAREA_AVG[application_train$NONLIVINGAREA_AVG < (qnt[1] - H)] <- caps[1]
application_train$NONLIVINGAREA_AVG[application_train$NONLIVINGAREA_AVG > (qnt[2] + H)] <- caps[2]

summary(application_train$APARTMENTS_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$APARTMENTS_MODE, main="APARTMENTS_MODE", boxwex=0.1)
#boxplot.stats(application_train$APARTMENTS_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$APARTMENTS_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$APARTMENTS_MODE[application_train$APARTMENTS_MODE < (qnt[1] - H)] <- caps[1]
application_train$APARTMENTS_MODE[application_train$APARTMENTS_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$BASEMENTAREA_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$BASEMENTAREA_MODE, main="BASEMENTAREA_MODE", boxwex=0.1)
#boxplot.stats(application_train$BASEMENTAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$BASEMENTAREA_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$BASEMENTAREA_MODE[application_train$BASEMENTAREA_MODE < (qnt[1] - H)] <- caps[1]
application_train$BASEMENTAREA_MODE[application_train$BASEMENTAREA_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$YEARS_BEGINEXPLUATATION_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$YEARS_BEGINEXPLUATATION_MODE, main="YEARS_BEGINEXPLUATATION_MODE", boxwex=0.1)
#boxplot.stats(application_train$YEARS_BEGINEXPLUATATION_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$YEARS_BEGINEXPLUATATION_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$YEARS_BEGINEXPLUATATION_MODE[application_train$YEARS_BEGINEXPLUATATION_MODE < (qnt[1] - H)] <- caps[1]
application_train$YEARS_BEGINEXPLUATATION_MODE[application_train$YEARS_BEGINEXPLUATATION_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$YEARS_BUILD_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$YEARS_BUILD_MODE, main="YEARS_BUILD_MODE", boxwex=0.1)
#boxplot.stats(application_train$YEARS_BUILD_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$YEARS_BUILD_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$YEARS_BUILD_MODE[application_train$YEARS_BUILD_MODE < (qnt[1] - H)] <- caps[1]
application_train$YEARS_BUILD_MODE[application_train$YEARS_BUILD_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$COMMONAREA_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$COMMONAREA_MODE, main="COMMONAREA_MODE", boxwex=0.1)
#boxplot.stats(application_train$COMMONAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$COMMONAREA_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$COMMONAREA_MODE[application_train$COMMONAREA_MODE < (qnt[1] - H)] <- caps[1]
application_train$COMMONAREA_MODE[application_train$COMMONAREA_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$ELEVATORS_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$ELEVATORS_MODE, main="ELEVATORS_MODE", boxwex=0.1)
#boxplot.stats(application_train$ELEVATORS_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$ELEVATORS_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$ELEVATORS_MODE[application_train$ELEVATORS_MODE < (qnt[1] - H)] <- caps[1]
application_train$ELEVATORS_MODE[application_train$ELEVATORS_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$ENTRANCES_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$ENTRANCES_MODE, main="ENTRANCES_MODE", boxwex=0.1)
#boxplot.stats(application_train$ENTRANCES_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$ENTRANCES_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$ENTRANCES_MODE[application_train$ENTRANCES_MODE < (qnt[1] - H)] <- caps[1]
application_train$ENTRANCES_MODE[application_train$ENTRANCES_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$FLOORSMAX_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$FLOORSMAX_MODE, main="FLOORSMAX_MODE", boxwex=0.1)
#boxplot.stats(application_train$FLOORSMAX_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$FLOORSMAX_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$FLOORSMAX_MODE[application_train$FLOORSMAX_MODE < (qnt[1] - H)] <- caps[1]
application_train$FLOORSMAX_MODE[application_train$FLOORSMAX_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$FLOORSMIN_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$FLOORSMIN_MODE, main="FLOORSMIN_MODE", boxwex=0.1)
#boxplot.stats(application_train$FLOORSMIN_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$FLOORSMIN_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$FLOORSMIN_MODE[application_train$FLOORSMIN_MODE < (qnt[1] - H)] <- caps[1]
application_train$FLOORSMIN_MODE[application_train$FLOORSMIN_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$LANDAREA_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$LANDAREA_MODE, main="LANDAREA_MODE", boxwex=0.1)
#boxplot.stats(application_train$LANDAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LANDAREA_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LANDAREA_MODE[application_train$LANDAREA_MODE < (qnt[1] - H)] <- caps[1]
application_train$LANDAREA_MODE[application_train$LANDAREA_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$LIVINGAPARTMENTS_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$LIVINGAPARTMENTS_MODE, main="LIVINGAPARTMENTS_MODE", boxwex=0.1)
#boxplot.stats(application_train$LIVINGAPARTMENTS_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LIVINGAPARTMENTS_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LIVINGAPARTMENTS_MODE[application_train$LIVINGAPARTMENTS_MODE < (qnt[1] - H)] <- caps[1]
application_train$LIVINGAPARTMENTS_MODE[application_train$LIVINGAPARTMENTS_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$LIVINGAREA_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$LIVINGAREA_MODE, main="LIVINGAREA_MODE", boxwex=0.1)
#boxplot.stats(application_train$LIVINGAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LIVINGAREA_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LIVINGAREA_MODE[application_train$LIVINGAREA_MODE < (qnt[1] - H)] <- caps[1]
application_train$LIVINGAREA_MODE[application_train$LIVINGAREA_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$NONLIVINGAPARTMENTS_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$NONLIVINGAPARTMENTS_MODE, main="NONLIVINGAPARTMENTS_MODE", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAPARTMENTS_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$NONLIVINGAPARTMENTS_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$NONLIVINGAPARTMENTS_MODE[application_train$NONLIVINGAPARTMENTS_MODE < (qnt[1] - H)] <- caps[1]
application_train$NONLIVINGAPARTMENTS_MODE[application_train$NONLIVINGAPARTMENTS_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$NONLIVINGAREA_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$NONLIVINGAREA_MODE, main="NONLIVINGAREA_MODE", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$NONLIVINGAREA_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$NONLIVINGAREA_MODE[application_train$NONLIVINGAREA_MODE < (qnt[1] - H)] <- caps[1]
application_train$NONLIVINGAREA_MODE[application_train$NONLIVINGAREA_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$APARTMENTS_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$APARTMENTS_MEDI, main="APARTMENTS_MEDI", boxwex=0.1)
#boxplot.stats(application_train$APARTMENTS_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$APARTMENTS_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$APARTMENTS_MEDI[application_train$APARTMENTS_MEDI < (qnt[1] - H)] <- caps[1]
application_train$APARTMENTS_MEDI[application_train$APARTMENTS_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$BASEMENTAREA_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$BASEMENTAREA_MEDI, main="BASEMENTAREA_MEDI", boxwex=0.1)
#boxplot.stats(application_train$BASEMENTAREA_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$BASEMENTAREA_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$BASEMENTAREA_MEDI[application_train$BASEMENTAREA_MEDI < (qnt[1] - H)] <- caps[1]
application_train$BASEMENTAREA_MEDI[application_train$BASEMENTAREA_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$YEARS_BEGINEXPLUATATION_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$YEARS_BEGINEXPLUATATION_MEDI, main="YEARS_BEGINEXPLUATATION_MEDI", boxwex=0.1)
#boxplot.stats(application_train$YEARS_BEGINEXPLUATATION_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$YEARS_BEGINEXPLUATATION_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$YEARS_BEGINEXPLUATATION_MEDI[application_train$YEARS_BEGINEXPLUATATION_MEDI < (qnt[1] - H)] <- caps[1]
application_train$YEARS_BEGINEXPLUATATION_MEDI[application_train$YEARS_BEGINEXPLUATATION_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$YEARS_BUILD_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$YEARS_BUILD_MEDI, main="YEARS_BUILD_MEDI", boxwex=0.1)
#boxplot.stats(application_train$YEARS_BUILD_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$YEARS_BUILD_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$YEARS_BUILD_MEDI[application_train$YEARS_BUILD_MEDI < (qnt[1] - H)] <- caps[1]
application_train$YEARS_BUILD_MEDI[application_train$YEARS_BUILD_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$COMMONAREA_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$COMMONAREA_MEDI, main="COMMONAREA_MEDI", boxwex=0.1)
#boxplot.stats(application_train$COMMONAREA_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$COMMONAREA_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$COMMONAREA_MEDI[application_train$COMMONAREA_MEDI < (qnt[1] - H)] <- caps[1]
application_train$COMMONAREA_MEDI[application_train$COMMONAREA_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$ELEVATORS_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$ELEVATORS_MEDI, main="ELEVATORS_MEDI", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$ELEVATORS_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$ELEVATORS_MEDI[application_train$ELEVATORS_MEDI < (qnt[1] - H)] <- caps[1]
application_train$ELEVATORS_MEDI[application_train$ELEVATORS_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$ENTRANCES_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$ENTRANCES_MEDI, main="ENTRANCES_MEDI", boxwex=0.1)
#boxplot.stats(application_train$ENTRANCES_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$ENTRANCES_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$ENTRANCES_MEDI[application_train$ENTRANCES_MEDI < (qnt[1] - H)] <- caps[1]
application_train$ENTRANCES_MEDI[application_train$ENTRANCES_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$FLOORSMAX_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$FLOORSMAX_MEDI, main="FLOORSMAX_MEDI", boxwex=0.1)
#boxplot.stats(application_train$FLOORSMAX_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$FLOORSMAX_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$FLOORSMAX_MEDI[application_train$FLOORSMAX_MEDI < (qnt[1] - H)] <- caps[1]
application_train$FLOORSMAX_MEDI[application_train$FLOORSMAX_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$FLOORSMIN_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$FLOORSMIN_MEDI, main="FLOORSMIN_MEDI", boxwex=0.1)
#boxplot.stats(application_train$FLOORSMIN_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$FLOORSMIN_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$FLOORSMIN_MEDI[application_train$FLOORSMIN_MEDI < (qnt[1] - H)] <- caps[1]
application_train$FLOORSMIN_MEDI[application_train$FLOORSMIN_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$LANDAREA_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$LANDAREA_MEDI, main="LANDAREA_MEDI", boxwex=0.1)
#boxplot.stats(application_train$LANDAREA_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LANDAREA_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LANDAREA_MEDI[application_train$LANDAREA_MEDI < (qnt[1] - H)] <- caps[1]
application_train$LANDAREA_MEDI[application_train$LANDAREA_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$LIVINGAPARTMENTS_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$LIVINGAPARTMENTS_MEDI, main="LIVINGAPARTMENTS_MEDI", boxwex=0.1)
#boxplot.stats(application_train$LIVINGAPARTMENTS_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LIVINGAPARTMENTS_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LIVINGAPARTMENTS_MEDI[application_train$LIVINGAPARTMENTS_MEDI < (qnt[1] - H)] <- caps[1]
application_train$LIVINGAPARTMENTS_MEDI[application_train$LIVINGAPARTMENTS_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$LIVINGAREA_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$LIVINGAREA_MEDI, main="LIVINGAREA_MEDI", boxwex=0.1)
#boxplot.stats(application_train$LIVINGAREA_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$LIVINGAREA_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$LIVINGAREA_MEDI[application_train$LIVINGAREA_MEDI < (qnt[1] - H)] <- caps[1]
application_train$LIVINGAREA_MEDI[application_train$LIVINGAREA_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$NONLIVINGAPARTMENTS_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$NONLIVINGAPARTMENTS_MEDI, main="NONLIVINGAPARTMENTS_MEDI", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAPARTMENTS_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$NONLIVINGAPARTMENTS_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$NONLIVINGAPARTMENTS_MEDI[application_train$NONLIVINGAPARTMENTS_MEDI < (qnt[1] - H)] <- caps[1]
application_train$NONLIVINGAPARTMENTS_MEDI[application_train$NONLIVINGAPARTMENTS_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$NONLIVINGAREA_MEDI)
# Check for outliers - Univariate approach
boxplot(application_train$NONLIVINGAREA_MEDI, main="NONLIVINGAREA_MEDI", boxwex=0.1)
#boxplot.stats(application_train$NONLIVINGAREA_MEDI)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$NONLIVINGAREA_MEDI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$NONLIVINGAREA_MEDI[application_train$NONLIVINGAREA_MEDI < (qnt[1] - H)] <- caps[1]
application_train$NONLIVINGAREA_MEDI[application_train$NONLIVINGAREA_MEDI > (qnt[2] + H)] <- caps[2]

summary(application_train$TOTALAREA_MODE)
# Check for outliers - Univariate approach
boxplot(application_train$TOTALAREA_MODE, main="TOTALAREA_MODE", boxwex=0.1)
#boxplot.stats(application_train$TOTALAREA_MODE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$TOTALAREA_MODE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$TOTALAREA_MODE[application_train$TOTALAREA_MODE < (qnt[1] - H)] <- caps[1]
application_train$TOTALAREA_MODE[application_train$TOTALAREA_MODE > (qnt[2] + H)] <- caps[2]

summary(application_train$OBS_30_CNT_SOCIAL_CIRCLE)
# Check for outliers - Univariate approach
boxplot(application_train$OBS_30_CNT_SOCIAL_CIRCLE, main="OBS_30_CNT_SOCIAL_CIRCLE", boxwex=0.1)
#boxplot.stats(application_train$OBS_30_CNT_SOCIAL_CIRCLE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$OBS_30_CNT_SOCIAL_CIRCLE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #changed the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$OBS_30_CNT_SOCIAL_CIRCLE[application_train$OBS_30_CNT_SOCIAL_CIRCLE < (qnt[1] - H)] <- caps[1]
application_train$OBS_30_CNT_SOCIAL_CIRCLE[application_train$OBS_30_CNT_SOCIAL_CIRCLE > (qnt[2] + H)] <- caps[2]

summary(application_train$DEF_30_CNT_SOCIAL_CIRCLE)
# Check for outliers - Univariate approach
boxplot(application_train$DEF_30_CNT_SOCIAL_CIRCLE, main="DEF_30_CNT_SOCIAL_CIRCLE", boxwex=0.1)
#boxplot.stats(application_train$DEF_30_CNT_SOCIAL_CIRCLE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$DEF_30_CNT_SOCIAL_CIRCLE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #changed the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$DEF_30_CNT_SOCIAL_CIRCLE[application_train$DEF_30_CNT_SOCIAL_CIRCLE < (qnt[1] - H)] <- caps[1]
application_train$DEF_30_CNT_SOCIAL_CIRCLE[application_train$DEF_30_CNT_SOCIAL_CIRCLE > (qnt[2] + H)] <- caps[2]

summary(application_train$OBS_60_CNT_SOCIAL_CIRCLE)
# Check for outliers - Univariate approach
boxplot(application_train$OBS_60_CNT_SOCIAL_CIRCLE, main="OBS_60_CNT_SOCIAL_CIRCLE", boxwex=0.1)
#boxplot.stats(application_train$OBS_60_CNT_SOCIAL_CIRCLE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$OBS_60_CNT_SOCIAL_CIRCLE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$OBS_60_CNT_SOCIAL_CIRCLE[application_train$OBS_60_CNT_SOCIAL_CIRCLE < (qnt[1] - H)] <- caps[1]
application_train$OBS_60_CNT_SOCIAL_CIRCLE[application_train$OBS_60_CNT_SOCIAL_CIRCLE > (qnt[2] + H)] <- caps[2]

summary(application_train$DEF_60_CNT_SOCIAL_CIRCLE)
# Check for outliers - Univariate approach
boxplot(application_train$DEF_60_CNT_SOCIAL_CIRCLE, main="DEF_60_CNT_SOCIAL_CIRCLE", boxwex=0.1)
#boxplot.stats(application_train$DEF_60_CNT_SOCIAL_CIRCLE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$DEF_60_CNT_SOCIAL_CIRCLE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$DEF_60_CNT_SOCIAL_CIRCLE[application_train$DEF_60_CNT_SOCIAL_CIRCLE < (qnt[1] - H)] <- caps[1]
application_train$DEF_60_CNT_SOCIAL_CIRCLE[application_train$DEF_60_CNT_SOCIAL_CIRCLE > (qnt[2] + H)] <- caps[2]

summary(application_train$DAYS_LAST_PHONE_CHANGE)
# Check for outliers - Univariate approach
boxplot(application_train$DAYS_LAST_PHONE_CHANGE, main="DAYS_LAST_PHONE_CHANGE", boxwex=0.1)
#boxplot.stats(application_train$DAYS_LAST_PHONE_CHANGE)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$DAYS_LAST_PHONE_CHANGE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$DAYS_LAST_PHONE_CHANGE[application_train$DAYS_LAST_PHONE_CHANGE < (qnt[1] - H)] <- caps[1]
application_train$DAYS_LAST_PHONE_CHANGE[application_train$DAYS_LAST_PHONE_CHANGE > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_REQ_CREDIT_BUREAU_HOUR)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_REQ_CREDIT_BUREAU_HOUR, main="AMT_REQ_CREDIT_BUREAU_HOUR", boxwex=0.1)
#boxplot.stats(application_train$AMT_REQ_CREDIT_BUREAU_HOUR)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_REQ_CREDIT_BUREAU_HOUR
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_REQ_CREDIT_BUREAU_HOUR[application_train$AMT_REQ_CREDIT_BUREAU_HOUR < (qnt[1] - H)] <- caps[1]
application_train$AMT_REQ_CREDIT_BUREAU_HOUR[application_train$AMT_REQ_CREDIT_BUREAU_HOUR > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_REQ_CREDIT_BUREAU_DAY)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_REQ_CREDIT_BUREAU_DAY, main="AMT_REQ_CREDIT_BUREAU_DAY", boxwex=0.1)
#boxplot.stats(application_train$AMT_REQ_CREDIT_BUREAU_DAY)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_REQ_CREDIT_BUREAU_DAY
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_REQ_CREDIT_BUREAU_DAY[application_train$AMT_REQ_CREDIT_BUREAU_DAY < (qnt[1] - H)] <- caps[1]
application_train$AMT_REQ_CREDIT_BUREAU_DAY[application_train$AMT_REQ_CREDIT_BUREAU_DAY > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_REQ_CREDIT_BUREAU_WEEK)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_REQ_CREDIT_BUREAU_WEEK, main="AMT_REQ_CREDIT_BUREAU_WEEK", boxwex=0.1)
#boxplot.stats(application_train$AMT_REQ_CREDIT_BUREAU_WEEK)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_REQ_CREDIT_BUREAU_WEEK
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_REQ_CREDIT_BUREAU_WEEK[application_train$AMT_REQ_CREDIT_BUREAU_WEEK < (qnt[1] - H)] <- caps[1]
application_train$AMT_REQ_CREDIT_BUREAU_WEEK[application_train$AMT_REQ_CREDIT_BUREAU_WEEK > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_REQ_CREDIT_BUREAU_MON)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_REQ_CREDIT_BUREAU_MON, main="AMT_REQ_CREDIT_BUREAU_MON", boxwex=0.1)
#boxplot.stats(application_train$AMT_REQ_CREDIT_BUREAU_MON)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_REQ_CREDIT_BUREAU_MON
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_REQ_CREDIT_BUREAU_MON[application_train$AMT_REQ_CREDIT_BUREAU_MON < (qnt[1] - H)] <- caps[1]
application_train$AMT_REQ_CREDIT_BUREAU_MON[application_train$AMT_REQ_CREDIT_BUREAU_MON > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_REQ_CREDIT_BUREAU_QRT)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_REQ_CREDIT_BUREAU_QRT, main="AMT_REQ_CREDIT_BUREAU_QRT", boxwex=0.1)
#boxplot.stats(application_train$AMT_REQ_CREDIT_BUREAU_QRT)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_REQ_CREDIT_BUREAU_QRT
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_REQ_CREDIT_BUREAU_QRT[application_train$AMT_REQ_CREDIT_BUREAU_QRT < (qnt[1] - H)] <- caps[1]
application_train$AMT_REQ_CREDIT_BUREAU_QRT[application_train$AMT_REQ_CREDIT_BUREAU_QRT > (qnt[2] + H)] <- caps[2]

summary(application_train$AMT_REQ_CREDIT_BUREAU_YEAR)
# Check for outliers - Univariate approach
boxplot(application_train$AMT_REQ_CREDIT_BUREAU_YEAR, main="AMT_REQ_CREDIT_BUREAU_YEAR", boxwex=0.1)
#boxplot.stats(application_train$AMT_REQ_CREDIT_BUREAU_YEAR)$out

#---> treat outliers: capping
# For missing values that lie outside the 1.5 * IQR limits, 
# we can cap it by replacing those observations outside the lower limit 
# with the value of 5th percentile and those that lie above the upper limit, 
# with the value of 95th percentile
x <- application_train$AMT_REQ_CREDIT_BUREAU_YEAR
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.01, .99), na.rm = T) #change the threshold
H <- 1.5 * IQR(x, na.rm = T)
print(qnt)
print(caps)
print(H)
application_train$AMT_REQ_CREDIT_BUREAU_YEAR[application_train$AMT_REQ_CREDIT_BUREAU_YEAR < (qnt[1] - H)] <- caps[1]
application_train$AMT_REQ_CREDIT_BUREAU_YEAR[application_train$AMT_REQ_CREDIT_BUREAU_YEAR > (qnt[2] + H)] <- caps[2]

# Next Up: Check for invalid levels/categories

# Check categorical levels for any invalid values
levels(application_train$CODE_GENDER) 
# XNA is invalid --> NA
application_train$CODE_GENDER[application_train$CODE_GENDER == 'XNA'] <- NA
application_train$CODE_GENDER<-factor(application_train$CODE_GENDER)

# Check categorical levels for any invalid values
levels(application_train$NAME_TYPE_SUITE)
# '' is invalid --> NA
application_train$NAME_TYPE_SUITE[application_train$NAME_TYPE_SUITE == ''] <- NA
application_train$NAME_TYPE_SUITE<-factor(application_train$NAME_TYPE_SUITE)

# Check categorical levels for any invalid values
levels(application_train$OCCUPATION_TYPE)
# '' is invalid --> NA
application_train$OCCUPATION_TYPE[application_train$OCCUPATION_TYPE == ''] <- NA
application_train$OCCUPATION_TYPE<-factor(application_train$OCCUPATION_TYPE)

# Check categorical levels for any invalid values
levels(application_train$ORGANIZATION_TYPE)
# XNA is invalid --> NA
application_train$ORGANIZATION_TYPE[application_train$ORGANIZATION_TYPE == 'XNA'] <- NA
application_train$ORGANIZATION_TYPE<-factor(application_train$ORGANIZATION_TYPE)

# Check categorical levels for any invalid values
levels(application_train$FONDKAPREMONT_MODE)
# '' is invalid --> NA
application_train$FONDKAPREMONT_MODE[application_train$FONDKAPREMONT_MODE == ''] <- NA
application_train$FONDKAPREMONT_MODE<-factor(application_train$FONDKAPREMONT_MODE)

# Check categorical levels for any invalid values
levels(application_train$HOUSETYPE_MODE)
# '' is invalid --> NA
application_train$HOUSETYPE_MODE[application_train$HOUSETYPE_MODE == ''] <- NA
application_train$HOUSETYPE_MODE<-factor(application_train$HOUSETYPE_MODE)

# Check categorical levels for any invalid values
levels(application_train$WALLSMATERIAL_MODE)
# '' is invalid --> NA
application_train$WALLSMATERIAL_MODE[application_train$WALLSMATERIAL_MODE == ''] <- NA
application_train$WALLSMATERIAL_MODE<-factor(application_train$WALLSMATERIAL_MODE)

# Check categorical levels for any invalid values
levels(application_train$EMERGENCYSTATE_MODE)
# '' is invalid --> NA
application_train$EMERGENCYSTATE_MODE[application_train$EMERGENCYSTATE_MODE == ''] <- NA
application_train$EMERGENCYSTATE_MODE<-factor(application_train$EMERGENCYSTATE_MODE)


# Next Up: Reducing Categories

# Check categorical levels for too many levels
levels(application_train$OCCUPATION_TYPE)
# review their distributions
table(application_train$OCCUPATION_TYPE)
#1. Laborers, Low-skill Laborers can be grouped together as Labourers
#2. Accountants, High skill tech staff, HR staff, IT staff, Managers, Secretaries can be grouped together as Corporate Staff
#3. Core staff, Cleaning staff, Cooking staff, Security staff, Waiters/barmen staff, Medicine staff can be grouped together as Core staff
levels(application_train$OCCUPATION_TYPE)[c(9,10)] <- "Labourers"
levels(application_train$OCCUPATION_TYPE)[c(1,6,7,8,10,15)] <- "Corporate staff"
levels(application_train$OCCUPATION_TYPE)[c(2,3,4,7,11,12)] <- "Core staff"
levels(application_train$OCCUPATION_TYPE)
application_train$OCCUPATION_TYPE<-factor(application_train$OCCUPATION_TYPE)

# Check categorical levels for too many levels
levels(application_train$ORGANIZATION_TYPE)
# review their distributions
table(application_train$ORGANIZATION_TYPE)
# 1. The different "type" categories of Transport, Trade, Industry can be grouped as one respectively
# 2. Kindergarten, School, University can be grouped together as Education
# 3. Hotel, Restaurant can be grouped together as Hospitality
# 4. Security Ministries, Postal, Police, Military, Government can be grouped together as Government
# 5. Construction, Housing, Realtor can be grouped together as Real Estate
# 6. Advertising, Bank, Cleaning, Electricity, Emergency, Insurance, Legal Services, Medicine, Security can be grouped together as Services
# 7. Culture, Religion, Other can be grouped together as Other
# 8. Mobile, Telecom can be grouped together as Mobile & Telecom
levels(application_train$ORGANIZATION_TYPE)[c(15:27)] <- "Industrial"
levels(application_train$ORGANIZATION_TYPE)[c(34:40)] <- "Trading"
levels(application_train$ORGANIZATION_TYPE)[c(35:38)] <- "Transport"
levels(application_train$ORGANIZATION_TYPE)[c(17,28,36)] <- "Education"
levels(application_train$ORGANIZATION_TYPE)[c(13,27)] <- "Hospitality"
levels(application_train$ORGANIZATION_TYPE)[c(12,20,23,24,28)] <- "Government"
levels(application_train$ORGANIZATION_TYPE)[c(8,14,22)] <- "Real Estate"
levels(application_train$ORGANIZATION_TYPE)[c(1,3,7,10,11,15,17,18,22)] <- "Services"
levels(application_train$ORGANIZATION_TYPE)[c(7,13,14)] <- "Other"
levels(application_train$ORGANIZATION_TYPE)[c(12,14)] <- "Mobile & Telecom"
levels(application_train$ORGANIZATION_TYPE)
application_train$ORGANIZATION_TYPE<-factor(application_train$ORGANIZATION_TYPE)


# Next Up: Missing Value treatment
### Check for null or missing values
cbind(sort(colSums(is.na(application_train)),decreasing=TRUE))
### Check for null or missing values percentage
sort(sapply(application_train, function(x) sum(is.na(x))/length(x))*100,decreasing=TRUE)
# Caution: We cannot afford to delete observations as we have an imbalanced dataset and 
# in doing so we might lose out on minority class representation.
# 1. We can see that there are many features with very high missing percentage. 
# Further looking into the definition of these features, we realize most of them are basically 
# the normalized information about the building where the client lives. Also, it maybe noted that 
# these features could very well be MNAR (missing not at random) as probably the data gathering 
# process, about the buildings the customers reside in, could either be incomplete or infeasible. 
# Hence, we can drop these features and not attempt a meaningless imputation.
# 2. EXT_SOURCE_1 is an important normalized score, hence we will not drop it even though it has 
# got a very high missing %. Same is the case for OCCUPATION_TYPE, EXT_SOURCE_3, ORGANIZATION_TYPE 
# and other such features. We will apply appropriate imputations for these.
# 3. OBS_30_CNT_SOCIAL_CIRCLE, DEF_30_CNT_SOCIAL_CIRCLE, OBS_60_CNT_SOCIAL_CIRCLE, 
# DEF_60_CNT_SOCIAL_CIRCLE could most likely be MNAR and hence, we will replace the NAs with 0 
# assuming they mean there has been no reported observations of delinquency
# 4. DAYS_LAST_PHONE_CHANGE could be either MAR (missing at random) or MNAR. We will replace the 
# NAs with min assuming the phone was not changed recently
# 5. CODE_GENDER could be MNAR and the client don't identify themselves as Male of Female. 
# We will mark them as 'X' (not specified/third gender)
# 6. AMT_REQ_CREDITBUREAU{x} could be MNAR implying no bureau enquiries for the customer in the 
# past year. We will replace NAs with 0
# 7. DAYS_EMPLOYED has 18% missing values and this is MNAR as the customer could be Unemployed or 
# a Pensioner. We can confirm the same by checking the dataset. If the person is employed at the 
# time of the application, DAYS_EMPLOYED is a negative value. So, a positive value will indicate 
# that the person is not employed at the moment of the application. Hence, we will mark the NAs 
# with positive(min(DAYS_EMPLOYED))
# 8. Remaining missing value feature can be assumed as MCAR (missing completely at random) or MAR. 
# We will impute them with median and mode
library(dplyr)
application_train = select(application_train, -COMMONAREA_AVG, 
                           -COMMONAREA_MODE, -COMMONAREA_MEDI, 
                           -NONLIVINGAPARTMENTS_AVG, -NONLIVINGAPARTMENTS_MODE,
                           -NONLIVINGAPARTMENTS_MEDI, -FONDKAPREMONT_MODE,
                           -LIVINGAPARTMENTS_AVG, -LIVINGAPARTMENTS_MODE,
                           -LIVINGAPARTMENTS_MEDI, -FLOORSMIN_AVG,
                           -FLOORSMIN_MODE, -FLOORSMIN_MEDI, -YEARS_BUILD_AVG,
                           -YEARS_BUILD_MODE, -YEARS_BUILD_MEDI, -OWN_CAR_AGE,
                           -LANDAREA_AVG, -LANDAREA_MODE, -LANDAREA_MEDI,
                           -BASEMENTAREA_AVG, -BASEMENTAREA_MODE, -BASEMENTAREA_MEDI,
                           -NONLIVINGAREA_AVG, -NONLIVINGAREA_MODE, -NONLIVINGAREA_MEDI,
                           -ELEVATORS_AVG, -ELEVATORS_MODE, -ELEVATORS_MEDI,
                           -WALLSMATERIAL_MODE, -APARTMENTS_AVG, -APARTMENTS_MODE,
                           -APARTMENTS_MEDI, -ENTRANCES_AVG, -ENTRANCES_MODE,
                           -ENTRANCES_MEDI, -LIVINGAREA_AVG, -LIVINGAREA_MODE,
                           -LIVINGAREA_MEDI, -HOUSETYPE_MODE, -FLOORSMAX_AVG,
                           -FLOORSMAX_MODE, -FLOORSMAX_MEDI, -YEARS_BEGINEXPLUATATION_AVG,
                           -YEARS_BEGINEXPLUATATION_MODE, -YEARS_BEGINEXPLUATATION_MEDI,
                           -TOTALAREA_MODE, -EMERGENCYSTATE_MODE)
dim(application_train)
#Treat OBS_30_CNT_SOCIAL_CIRCLE, DEF_30_CNT_SOCIAL_CIRCLE, OBS_60_CNT_SOCIAL_CIRCLE, 
#DEF_60_CNT_SOCIAL_CIRCLE
library(Hmisc)
application_train$OBS_30_CNT_SOCIAL_CIRCLE <- impute(application_train$OBS_30_CNT_SOCIAL_CIRCLE, 0)
application_train$DEF_30_CNT_SOCIAL_CIRCLE <- impute(application_train$DEF_30_CNT_SOCIAL_CIRCLE, 0)
application_train$OBS_60_CNT_SOCIAL_CIRCLE <- impute(application_train$OBS_60_CNT_SOCIAL_CIRCLE, 0)
application_train$DEF_60_CNT_SOCIAL_CIRCLE <- impute(application_train$DEF_60_CNT_SOCIAL_CIRCLE, 0)

# Treat DAYS_LAST_PHONE_CHANGE
application_train$DAYS_LAST_PHONE_CHANGE <- impute(application_train$DAYS_LAST_PHONE_CHANGE, min)

# Treat CODE_GENDER
application_train$CODE_GENDER <- impute(application_train$CODE_GENDER, 'X')
application_train$CODE_GENDER<-factor(application_train$CODE_GENDER)

# Treat AMT_REQ_CREDIT_BUREAU_HOUR, AMT_REQ_CREDIT_BUREAU_DAY, AMT_REQ_CREDIT_BUREAU_WEEK
# AMT_REQ_CREDIT_BUREAU_MON, AMT_REQ_CREDIT_BUREAU_QRT, AMT_REQ_CREDIT_BUREAU_YEAR
application_train$AMT_REQ_CREDIT_BUREAU_HOUR <- impute(application_train$AMT_REQ_CREDIT_BUREAU_HOUR, 0)
application_train$AMT_REQ_CREDIT_BUREAU_DAY <- impute(application_train$AMT_REQ_CREDIT_BUREAU_DAY, 0)
application_train$AMT_REQ_CREDIT_BUREAU_WEEK <- impute(application_train$AMT_REQ_CREDIT_BUREAU_WEEK, 0)
application_train$AMT_REQ_CREDIT_BUREAU_MON <- impute(application_train$AMT_REQ_CREDIT_BUREAU_MON, 0)
application_train$AMT_REQ_CREDIT_BUREAU_QRT <- impute(application_train$AMT_REQ_CREDIT_BUREAU_QRT, 0)
application_train$AMT_REQ_CREDIT_BUREAU_YEAR <- impute(application_train$AMT_REQ_CREDIT_BUREAU_YEAR, 0)

# Treat DAYS_EMPLOYED
application_train$DAYS_EMPLOYED <- impute(application_train$DAYS_EMPLOYED, 11338)

# Treat EXT_SOURCE_1
application_train$EXT_SOURCE_1 <- impute(application_train$EXT_SOURCE_1, median)

# Treat OCCUPATION_TYPE
application_train$OCCUPATION_TYPE <- impute(application_train$OCCUPATION_TYPE, mode)

# Treat EXT_SOURCE_3
application_train$EXT_SOURCE_3 <- impute(application_train$EXT_SOURCE_3, median)

# Treat ORGANIZATION_TYPE
application_train$ORGANIZATION_TYPE <- impute(application_train$ORGANIZATION_TYPE, mode)

# Treat NAME_TYPE_SUITE
application_train$NAME_TYPE_SUITE <- impute(application_train$NAME_TYPE_SUITE, mode)

# Treat EXT_SOURCE_2
application_train$EXT_SOURCE_2 <- impute(application_train$EXT_SOURCE_2, median)

# Treat AMT_GOODS_PRICE
application_train$AMT_GOODS_PRICE <- impute(application_train$AMT_GOODS_PRICE, median)

# Treat AMT_ANNUITY
application_train$AMT_ANNUITY <- impute(application_train$AMT_ANNUITY, median)

# Treat CNT_FAM_MEMBERS
application_train$CNT_FAM_MEMBERS <- impute(application_train$CNT_FAM_MEMBERS, median)

table(is.na(application_train))
# Missing values treatment is done.

# Next Up: Data Transformation: Standardization (Scaling/Normalization - min/max or Z-score): 
# Feature scaling is not needed at this point of time as it will be neede for certain 
# algorithms/models but not for all. For example, we will apply scaling when doing say PCA but 
# most of the predictive modeling algorithms are already equipped to handle this or is immune, 
# e.g. tree based models are immune and LDA can handle varying ranges.

# Next Up: Data Transformation: Handling skewness (Log transformations) - we will use this later 
# to improve upon the baseline models.


# --- write to csv the new application train data
write.csv(application_train, "application_train_clean.csv",row.names = FALSE)
