rm(list=ls(all=TRUE))

bureau_data <- read.csv("Capstone/bureau.csv",1)
bureau_balance_data <- read.csv("Capstone/bureau_balance.csv",1)
# Note:
#  - There are 1716428 unique SK_ID_BUREAU values in the bureau data table 
#  - However, there are only 817395 unique SK_ID_BUREAU values in the bureau balance data table
#  - Also, pick the latest credit reported in bureau balance data table as there will be
#    multiple rows for the same SK_ID_BUREAU for all the reported months

#*** Handle missing values & outliers ***
#http://r-statistics.co/Missing-Value-Treatment-With-R.html
#https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#http://r-statistics.co/Outlier-Treatment-With-R.html
#https://www.neuraldesigner.com/blog/3_methods_to_deal_with_outliers

#--> missing values - bureau balance data
cbind(colSums(is.na(bureau_balance_data))) # No Missing Values
#library(mice)
#md.pattern(bureau_balance_data)
#--> outliers - bureau balance data
summary(bureau_balance_data)
boxplot(bureau_balance_data$MONTHS_BALANCE, main="MONTHS_BALANCE", boxwex=0.1)
boxplot.stats(bureau_balance_data$MONTHS_BALANCE)$out
# No Outliers

#--> missing values - bureau data
cbind(colSums(is.na(bureau_data))) # there are missing values
#DAYS_CREDIT_ENDDATE     105553 6.14% (MCAR: missing completely at random)
#DAYS_ENDDATE_FACT       633653 36.92% - (MNAR: missing not at random) only available for Closed loans, NA otherwise
#AMT_CREDIT_MAX_OVERDUE 1124488 65.51% - MCAR
#AMT_CREDIT_SUM              13 0.00076% - MCAR
#AMT_CREDIT_SUM_DEBT     257669 15.01% - MCAR
#AMT_CREDIT_SUM_LIMIT    591780 34.48% - (MNAR) applicable for credit cards
#AMT_ANNUITY            1226791 71.47% - MCAR
#--> outliers
summary(bureau_data) # there are outliers
# DAYS_CREDIT_ENDDATE - Min: -42060.0 (~115 years in past) Max: 31199.0 (~86 years in future)
# DAYS_ENDDATE_FACT - Min: -42023.0 (~115 years in past)
# AMT_CREDIT_MAX_OVERDUE - Max:115987185 - only ~7% of the total observations is above 0
# AMT_CREDIT_SUM - Max: 585000000
# AMT_CREDIT_SUM_DEBT - Min: -4705600 Max: 170100000
# AMT_CREDIT_SUM_LIMIT - Min: -586406 Max: 4705600
# DAYS_CREDIT_UPDATE - Min: -41947.0
# AMT_ANNUITY - Max: 118453424 - only 13.56% customers have pledged annuity
boxplot(bureau_data$DAYS_CREDIT_ENDDATE, main="DAYS_CREDIT_ENDDATE", boxwex=0.1)
boxplot.stats(bureau_data$DAYS_CREDIT_ENDDATE)$out
boxplot(bureau_data$DAYS_ENDDATE_FACT, main="DAYS_ENDDATE_FACT", boxwex=0.1)
boxplot.stats(bureau_data$DAYS_ENDDATE_FACT)$out
boxplot(bureau_data$DAYS_CREDIT_UPDATE, main="DAYS_CREDIT_UPDATE", boxwex=0.1)
boxplot.stats(bureau_data$DAYS_CREDIT_UPDATE)$out
# Note: for the amount features we will see if any outlier treatment is needed 
# basis the model results to see if we can achieve any improvement

#---> treat outliers: capping
x <- bureau_data$DAYS_CREDIT_ENDDATE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
bureau_data$DAYS_CREDIT_ENDDATE[bureau_data$DAYS_CREDIT_ENDDATE < (qnt[1] - H)] <- caps[1]
bureau_data$DAYS_CREDIT_ENDDATE[bureau_data$DAYS_CREDIT_ENDDATE > (qnt[2] + H)] <- caps[2]

x <- bureau_data$DAYS_ENDDATE_FACT
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
bureau_data$DAYS_ENDDATE_FACT[bureau_data$DAYS_ENDDATE_FACT < (qnt[1] - H)] <- caps[1]
bureau_data$DAYS_ENDDATE_FACT[bureau_data$DAYS_ENDDATE_FACT > (qnt[2] + H)] <- caps[2]

x <- bureau_data$DAYS_CREDIT_UPDATE
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
bureau_data$DAYS_CREDIT_UPDATE[bureau_data$DAYS_CREDIT_UPDATE < (qnt[1] - H)] <- caps[1]
bureau_data$DAYS_CREDIT_UPDATE[bureau_data$DAYS_CREDIT_UPDATE > (qnt[2] + H)] <- caps[2]

#---> treat missing values: imputation
library(Hmisc)
#impute(bureau_data$AMT_ANNUITY, mean)  # replace with mean
#impute(bureau_data$AMT_ANNUITY, median)  # median
#impute(bureau_data$AMT_ANNUITY, 0)  # replace specific number
# or impute manually
bureau_data$AMT_ANNUITY[is.na(bureau_data$AMT_ANNUITY)] <- 0

bureau_data$AMT_CREDIT_MAX_OVERDUE[is.na(bureau_data$AMT_CREDIT_MAX_OVERDUE)] <- bureau_data$AMT_CREDIT_SUM_OVERDUE[is.na(bureau_data$AMT_CREDIT_MAX_OVERDUE)]

bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE != 'Credit card'] <- 0
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active'] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active']
bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active'] <- bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active']
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Closed'] <- 0
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Sold'] <- 0
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Bad debt'] <- bureau_data$AMT_CREDIT_SUM_OVERDUE[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Bad debt']
bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM) & bureau_data$CREDIT_TYPE != 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Closed'] <- 0

# For Credit Card: Sum = Debt + Limit
bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card'] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card'] - bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card']
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card'] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card'] - bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card']
bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM) & bureau_data$CREDIT_TYPE == 'Credit card'] <- bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM) & bureau_data$CREDIT_TYPE == 'Credit card'] + bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM) & bureau_data$CREDIT_TYPE == 'Credit card']
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Closed'] <- 0
bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Closed'] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Closed']
# -- For Active Credit Cards will need to check STATUS in bureau_balance data before imputation
bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE == 0 & (bureau_data$AMT_CREDIT_SUM_DEBT == 0 | is.na(bureau_data$AMT_CREDIT_SUM_DEBT))] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE == 0 & (bureau_data$AMT_CREDIT_SUM_DEBT == 0 | is.na(bureau_data$AMT_CREDIT_SUM_DEBT))]
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE == 0 & (bureau_data$AMT_CREDIT_SUM_LIMIT == 0 | is.na(bureau_data$AMT_CREDIT_SUM_LIMIT))] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE == 0 & (bureau_data$AMT_CREDIT_SUM_LIMIT == 0 | is.na(bureau_data$AMT_CREDIT_SUM_LIMIT))]
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE == 0 & (bureau_data$AMT_CREDIT_SUM_LIMIT == bureau_data$AMT_CREDIT_SUM)] <- 0
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE != 0] <- bureau_data$AMT_CREDIT_SUM_OVERDUE[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Active' & bureau_data$AMT_CREDIT_SUM_OVERDUE != 0]
bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card'] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card'] - bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card']
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Sold'] <- 0
bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Sold'] <- 0
bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Bad debt' & bureau_data$AMT_CREDIT_SUM_OVERDUE != 0] <- bureau_data$AMT_CREDIT_SUM_OVERDUE[is.na(bureau_data$AMT_CREDIT_SUM_DEBT) & bureau_data$CREDIT_TYPE == 'Credit card' & bureau_data$CREDIT_ACTIVE == 'Bad debt' & bureau_data$AMT_CREDIT_SUM_OVERDUE != 0]
bureau_data$AMT_CREDIT_SUM_LIMIT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card'] <- bureau_data$AMT_CREDIT_SUM[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card'] - bureau_data$AMT_CREDIT_SUM_DEBT[is.na(bureau_data$AMT_CREDIT_SUM_LIMIT) & bureau_data$CREDIT_TYPE == 'Credit card']

bureau_data$DAYS_CREDIT_ENDDATE[is.na(bureau_data$DAYS_CREDIT_ENDDATE)] <- bureau_data$DAYS_ENDDATE_FACT[is.na(bureau_data$DAYS_CREDIT_ENDDATE)]  
bureau_data$DAYS_CREDIT_ENDDATE[is.na(bureau_data$DAYS_CREDIT_ENDDATE) & bureau_data$AMT_CREDIT_SUM_DEBT==0] <- 0  

# -- ignoring the remaining missing values in DAYS_CREDIT_ENDDATE & DAYS_ENDDATE_FACT
# 125 observations with closed status are missing DAYS_ENDDATE_FACT
# rest of the NAs (628638) are valid

# verify imputation
cbind(colSums(is.na(bureau_data)))
#*** Handled missing values & outliers ***

# Let's calculate the risk score first on the bureau balance data
temp_score <- bureau_balance_data
temp_score$STATUS_wt[temp_score$STATUS=='C'] <- -999
temp_score$STATUS_wt[temp_score$STATUS=='X'] <- 15
temp_score$STATUS_wt[temp_score$STATUS=='5'] <- 150
temp_score$STATUS_wt[temp_score$STATUS=='4'] <- 120
temp_score$STATUS_wt[temp_score$STATUS=='3'] <- 90
temp_score$STATUS_wt[temp_score$STATUS=='2'] <- 60
temp_score$STATUS_wt[temp_score$STATUS=='1'] <- 30
temp_score$STATUS_wt[temp_score$STATUS=='0'] <- 0

library(dplyr)
bureau_calc_score<-temp_score %>%
  group_by(SK_ID_BUREAU) %>%
  summarise(RISK_SCORE = mean(STATUS_wt))

# pick the latest credit reported in bureau balance data table
temp <- distinct(bureau_balance_data,SK_ID_BUREAU,.keep_all = TRUE)
bureau_balance_data <- temp

# merge the calculated risk scores with bureau balance data
bureau_balance_data <- merge(bureau_balance_data,bureau_calc_score,by="SK_ID_BUREAU",sort = FALSE,all.x=TRUE)

# merge the data sets
# merge the two bureau data frames by SK_ID_BUREAU
bureau_agg_data <- merge(bureau_data,bureau_balance_data,by="SK_ID_BUREAU",sort = FALSE,all.x=TRUE)
rm(bureau_balance_data)
rm(bureau_data)
rm(bureau_calc_score)
rm(temp)
rm(temp_score)

head(bureau_agg_data)

str(bureau_agg_data)
# missing values
cbind(colSums(is.na(bureau_agg_data)))
# missing values are the ones from bureau_balance data with no match in bureau_data

# Let's calculate the risk score first of the missing ones
temp_score <- bureau_agg_data[which(is.na(bureau_agg_data$MONTHS_BALANCE) & is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$RISK_SCORE)),]
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT)==FALSE] <- -999 # assuming STATUS=C
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT) & temp_score$CREDIT_DAY_OVERDUE == 0] <- 0 # assuming STATUS=0
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT) & temp_score$CREDIT_DAY_OVERDUE > 120] <- 150 # assuming STATUS=5
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT) & (temp_score$CREDIT_DAY_OVERDUE > 0 & temp_score$CREDIT_DAY_OVERDUE < 31)] <- 30 # assuming STATUS=1
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT) & (temp_score$CREDIT_DAY_OVERDUE > 30 & temp_score$CREDIT_DAY_OVERDUE < 61)] <- 60 # assuming STATUS=2
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT) & (temp_score$CREDIT_DAY_OVERDUE > 60 & temp_score$CREDIT_DAY_OVERDUE < 91)] <- 90 # assuming STATUS=3
temp_score$STATUS_wt[is.na(temp_score$DAYS_ENDDATE_FACT) & (temp_score$CREDIT_DAY_OVERDUE > 90 & temp_score$CREDIT_DAY_OVERDUE < 121)] <- 120 # assuming STATUS=4
bureau_calc_score<-temp_score %>%
  group_by(SK_ID_BUREAU) %>%
  summarise(RISK_SCORE = mean(STATUS_wt))

bureau_agg_data[is.na(bureau_agg_data$RISK_SCORE),"RISK_SCORE"] <- bureau_calc_score[bureau_calc_score$SK_ID_BUREAU %in% bureau_agg_data[is.na(bureau_agg_data$RISK_SCORE),"SK_ID_BUREAU"], "RISK_SCORE"]
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT)==FALSE] <- 'C'
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT) & bureau_agg_data$CREDIT_DAY_OVERDUE == 0] <- '0' 
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT) & bureau_agg_data$CREDIT_DAY_OVERDUE > 120] <- '5' 
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT) & (bureau_agg_data$CREDIT_DAY_OVERDUE > 0 & bureau_agg_data$CREDIT_DAY_OVERDUE < 31)] <- '1' 
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT) & (bureau_agg_data$CREDIT_DAY_OVERDUE > 30 & bureau_agg_data$CREDIT_DAY_OVERDUE < 61)] <- '2' 
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT) & (bureau_agg_data$CREDIT_DAY_OVERDUE > 60 & bureau_agg_data$CREDIT_DAY_OVERDUE < 91)] <- '3' 
bureau_agg_data$STATUS[is.na(bureau_agg_data$STATUS) & is.na(bureau_agg_data$DAYS_ENDDATE_FACT) & (bureau_agg_data$CREDIT_DAY_OVERDUE > 90 & bureau_agg_data$CREDIT_DAY_OVERDUE < 121)] <- '4' 

rm(bureau_calc_score)
rm(temp_score)

#library(DMwR)
#knnOutput <- knnImputation(bureau_agg_data)
#library(mice)
#miceMod <- mice(bureau_agg_data, method="rf")  # perform mice imputation, based on random forests.
#miceOutput <- complete(miceMod)  # generate the completed data.

summary(bureau_agg_data)

bureau_clean_data <- data.frame(SK_ID_CURR=unique(bureau_agg_data$SK_ID_CURR))
# - There are only 305811 unique SK_ID_CURR which we can use to relate to Home Credit data
# ************* Feature Creation *************************************************************

## Feature 1: Risk Score basis bureau balance data
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(score = mean(RISK_SCORE, na.rm = T))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "score"] <- "RISK_SCORE"

## Feature 2: Number of loans reported in bureau
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loans = n())
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loans"] <- "BUREAU_LOAN_COUNT"

## Feature 3: Number of loans by type
levels(bureau_agg_data$CREDIT_TYPE) # There are 15 type of loans
table(bureau_agg_data$CREDIT_TYPE)
# 'Another type of loan','Cash loan (non-earmarked)','Interbank credit',
# 'Mobile operator loan','Unknown type of loan' ==> 'Others'
levels(bureau_agg_data$CREDIT_TYPE)[c(1,3,6,12,15)] <- "Others"
# 'Loan for business development','Loan for purchase of shares (margin lending)',
# 'Loan for the purchase of equipment','Loan for working capital replenishment' ==> 'Business Loan'
levels(bureau_agg_data$CREDIT_TYPE)[c(5:8)] <- "Business Loan"
table(bureau_agg_data$CREDIT_TYPE)

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Consumer credit") %>%
  group_by(SK_ID_CURR,CREDIT_TYPE) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_CONSUMER_CREDIT_LOAN_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Car loan") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_CAR_LOAN_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Credit card") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_CREDIT_CARD_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Business Loan") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_BUSINESS_LOAN_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Microloan") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_MICRO_LOAN_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Mortgage") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_MORTGAGE_LOAN_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Real estate loan") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_REAL_ESTATE_LOAN_COUNT"

temp<-bureau_agg_data %>%
  filter(CREDIT_TYPE == "Others") %>%
  group_by(SK_ID_CURR) %>%
  summarise(number_loan_types = n())
bureau_clean_data <- merge(bureau_clean_data,temp[,c("SK_ID_CURR","number_loan_types")],by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "number_loan_types"] <- "BUREAU_OTHER_LOAN_COUNT"

# Set the counts to 0 for the NAs
bureau_clean_data$BUREAU_CONSUMER_CREDIT_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_CONSUMER_CREDIT_LOAN_COUNT)] <- 0
bureau_clean_data$BUREAU_CAR_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_CAR_LOAN_COUNT)] <- 0
bureau_clean_data$BUREAU_CREDIT_CARD_COUNT[is.na(bureau_clean_data$BUREAU_CREDIT_CARD_COUNT)] <- 0
bureau_clean_data$BUREAU_BUSINESS_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_BUSINESS_LOAN_COUNT)] <- 0
bureau_clean_data$BUREAU_MICRO_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_MICRO_LOAN_COUNT)] <- 0
bureau_clean_data$BUREAU_MORTGAGE_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_MORTGAGE_LOAN_COUNT)] <- 0
bureau_clean_data$BUREAU_REAL_ESTATE_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_REAL_ESTATE_LOAN_COUNT)] <- 0
bureau_clean_data$BUREAU_OTHER_LOAN_COUNT[is.na(bureau_clean_data$BUREAU_OTHER_LOAN_COUNT)] <- 0

## Feature 4: Total Annuity Amount for Bureau loans
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(total_annuity = sum(AMT_ANNUITY))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "total_annuity"] <- "BUREAU_TOTAL_AMT_ANNUITY"

## Feature 5: Bad Loan Ratio for Bureau loans
temp<-bureau_agg_data %>%
  filter(CREDIT_ACTIVE == "Bad debt" | CREDIT_ACTIVE == "Sold") %>%
  group_by(SK_ID_CURR) %>%
  summarise(bad_loans = n())
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
bureau_clean_data$bad_loans <- bureau_clean_data$bad_loans / bureau_clean_data$BUREAU_LOAN_COUNT
names(bureau_clean_data)[names(bureau_clean_data) == "bad_loans"] <- "BUREAU_BAD_DEBT_RATIO"

# Set to 0 for the NAs
bureau_clean_data$BUREAU_BAD_DEBT_RATIO[is.na(bureau_clean_data$BUREAU_BAD_DEBT_RATIO)] <- 0

## Feature 6: % Open debts as per Bureau
temp<-bureau_agg_data %>%
  filter(STATUS != "C") %>%
  group_by(SK_ID_CURR) %>%
  summarise(active_loans = n())
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
bureau_clean_data$active_loans <- bureau_clean_data$active_loans / bureau_clean_data$BUREAU_LOAN_COUNT
names(bureau_clean_data)[names(bureau_clean_data) == "active_loans"] <- "BUREAU_ACTIVE_DEBTS_RATIO"

# Set to 0 for the NAs meaning customers have only STATUS=C loans
bureau_clean_data$BUREAU_ACTIVE_DEBTS_RATIO[is.na(bureau_clean_data$BUREAU_ACTIVE_DEBTS_RATIO)] <- 0

## Feature 7: Latest CB Credit applied relative to Home Credit application
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(latest_loan = max(DAYS_CREDIT))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "latest_loan"] <- "BUREAU_LATEST_CREDIT"

## Feature 8: Total Overdue
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(total_overdue = sum(AMT_CREDIT_SUM_OVERDUE))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "total_overdue"] <- "BUREAU_TOTAL_AMT_OVERDUE"

## Feature 9: Max Overdue
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(max_overdue = max(AMT_CREDIT_MAX_OVERDUE))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "max_overdue"] <- "BUREAU_MAX_AMT_OVERDUE"

## Feature 10: Average Overdue days
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(days_overdue = mean(CREDIT_DAY_OVERDUE))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "days_overdue"] <- "BUREAU_DAYS_OVERDUE"

## Feature 11: Average Credit Prolonged Frequency
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(credit_prolonged_freq = mean(CNT_CREDIT_PROLONG))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "credit_prolonged_freq"] <- "BUREAU_CREDIT_PROLONGED_FREQ"

## Feature 12: Total Debt Outstanding
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(total_debt_outstanding = sum(AMT_CREDIT_SUM_DEBT))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "total_debt_outstanding"] <- "BUREAU_AMT_DEBT_OUTSTANDING"

## Feature 13: Overdue Debt Ratio
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(overdue_debt_ratio = sum(AMT_CREDIT_SUM_OVERDUE) / sum(AMT_CREDIT_SUM_DEBT))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "overdue_debt_ratio"] <- "BUREAU_OVERDUE_DEBT_RATIO"

# Set to 0 for the NANs because of no current Debts
bureau_clean_data$BUREAU_OVERDUE_DEBT_RATIO[is.na(bureau_clean_data$BUREAU_OVERDUE_DEBT_RATIO)] <- 0

## Feature 14: Debt over Credit Ratio
temp<-bureau_agg_data %>%
  group_by(SK_ID_CURR) %>%
  summarise(debt_over_credit_ratio = sum(AMT_CREDIT_SUM_DEBT) / sum(AMT_CREDIT_SUM))
bureau_clean_data <- merge(bureau_clean_data,temp,by="SK_ID_CURR",sort = FALSE,all.x=TRUE)
names(bureau_clean_data)[names(bureau_clean_data) == "debt_over_credit_ratio"] <- "BUREAU_DEBT_CREDIT_RATIO"

# Set to 0 for the NANs because of no current credit
bureau_clean_data$BUREAU_DEBT_CREDIT_RATIO[is.na(bureau_clean_data$BUREAU_DEBT_CREDIT_RATIO)] <- 0

# missing values
cbind(colSums(is.na(bureau_clean_data)))

# --- write to csv the new bureau data to merge to home credit application data
write.csv(bureau_clean_data, "bureau_data.csv",row.names = FALSE)
