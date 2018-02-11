# Author: Aaditya Anand (aaditya23a@gmail.com)

# Importing necessary libraries
library(corrplot)
library(ggplot2)
library(caret)
library(plyr)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(unbalanced)
library(MASS)
library(randomForest)
library(party)
library(car)
library(ROCR)
library(scales)
library(lsr)
library(boot)
library(pROC)
library(DMwR)


# Reading the training data and converting blanks to NA
customerdata <- read.csv("churn_training_data.csv", header=T, na.strings=c("","NA"))

churnoutput <- read.csv("churn_training_output.csv", header=T, na.strings=c("","NA"))


# Adding the goal variable - churn in customerdata
customerdata$churn <- churnoutput$churn[churnoutput$id == customerdata$id]
table(customerdata$churn) 

# Checking for Missing Values percentage across features
sapply(customerdata, function(x) sum(is.na(x))/length(x)*100)

########### DATA CLEANING AND PRE-PROCESSING ###########

# Removing features with >75% missing values
customerdata$campaign_disc_ele <- NULL
customerdata$date_first_activ <- NULL
customerdata$forecast_base_bill_ele <- NULL
customerdata$forecast_base_bill_year <- NULL
customerdata$forecast_bill_12m <- NULL
customerdata$forecast_cons <- NULL


# Treating Missing Values features 
# Blanks in activity_new recoded to 0 and the populated ones to 1
customerdata$activity_new  <- ifelse(is.na(customerdata$activity_new), 0, 1)
table(customerdata$activity_new)


# NAs and sparse frequency levels in origin_up and channel_sales recoded as Others, while the populated ones are recoded meaningfully

customerdata$origin_up <- ifelse(is.na(customerdata$origin_up),"Others", ifelse(customerdata$origin_up == "kamkkxfxxuwbdslkwifmmcsiusiuosws", "Campaign1", 
                                 ifelse(customerdata$origin_up == "ldkssxwpmemidmecebumciepifcamkci", "Campaign2", 
                                        ifelse(customerdata$origin_up == "lxidpiddsbxsbosboudacockeimpuepw", "Campaign3", "Others"))))

table(customerdata$origin_up)


customerdata$channel_sales <- ifelse(is.na(customerdata$channel_sales),"Others", ifelse(customerdata$channel_sales == "ewpakwlliwisiwduibdlfmalxowmwpci", "Channel1", 
                                     ifelse(customerdata$channel_sales == "foosdfpfkusacimwkcsosbicdxkicaua", "Channel2", 
                                            ifelse(customerdata$channel_sales == "lmkebamcaaclubfxadlmueccxoimlema", "Channel3", 
                                                   ifelse(customerdata$channel_sales == "usilxuppasemubllopkaafesmlibmsdf", "Channel4", "Others")))))

table(customerdata$channel_sales)


# Values from feature has_gas recoded from t and f to 1 and 0

customerdata$has_gas <- as.numeric(customerdata$has_gas)
customerdata$has_gas <- ifelse(customerdata$has_gas == 1, 0, 1)
table(customerdata$has_gas)


# Treating/transforming the Date columns
sum(is.na(customerdata$date_end))

customerdata <- customerdata[complete.cases(customerdata$date_end), ]
nrow(customerdata)


sum(is.na(customerdata$date_modif_prod))

# Replacing NAs in date_modif_prod with corresponding date_active values..
# ..assuming the contract signing as an activity, also it'd be the oldest activity technically
customerdata$date_modif_prod <- as.character(customerdata$date_modif_prod)
customerdata$date_modif_prod[is.na(customerdata$date_modif_prod)] <- 
  as.character(customerdata$date_activ[is.na(customerdata$date_modif_prod)])

sum(is.na(customerdata$date_renewal))

customerdata <- customerdata[complete.cases(customerdata$date_renewal), ]
nrow(customerdata)


# Assuming the baseline date for our analysis to be December 31, 2016 since data is as of Dec 2016
date_latest <- as.Date('2016-01-31', '%Y-%m-%d')

# Transforming date features to meaningful continous features 
# Days since active - Number of days since the date the contract became active (as of December 31, 2016)
customerdata$days_since_activ <- as.numeric(as.character(date_latest - as.Date(as.character(customerdata$date_activ), format="%Y-%m-%d")))

# Days since renewal - Number of days since contract renewal date (as of December 31, 2016)
customerdata$days_since_renewal <- as.numeric(as.character(date_latest - as.Date(as.character(customerdata$date_renewal), format="%Y-%m-%d")))

# Days since last modification - Number of days since the last modification done in the contract (as of December 31, 2016)
customerdata$days_since_modif <- as.numeric(as.character(date_latest - as.Date(as.character(customerdata$date_modif_prod), format="%Y-%m-%d")))

# Function to find mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sum(is.na(customerdata$forecast_discount_energy)) 
# Since forecast_discount_energy takes a limited set of values, assuming it be discrete and replacing NA values with its mode
customerdata$forecast_discount_energy[is.na(customerdata$forecast_discount_energy)] <- 
  getmode(customerdata$forecast_discount_energy)

# Checking NAs for the rest of the continuous features
sum(is.na(customerdata$forecast_price_energy_p1)) 
sum(is.na(customerdata$forecast_price_energy_p2)) 
sum(is.na(customerdata$forecast_price_pow_p1)) 
sum(is.na(customerdata$margin_gross_pow_ele)) 
sum(is.na(customerdata$margin_net_pow_ele)) 
sum(is.na(customerdata$net_margin)) 
sum(is.na(customerdata$pow_max))

# Treating missing values for the rest of the continuous features by imputing their means
customerdata$forecast_price_energy_p1[is.na(customerdata$forecast_price_energy_p1)] <- 
  median(customerdata$forecast_price_energy_p1, na.rm = T)

customerdata$forecast_price_energy_p2[is.na(customerdata$forecast_price_energy_p2)] <- 
  median(customerdata$forecast_price_energy_p2, na.rm = T)

customerdata$forecast_price_pow_p1[is.na(customerdata$forecast_price_pow_p1)] <- 
  median(customerdata$forecast_price_pow_p1, na.rm = T)

customerdata$margin_gross_pow_ele[is.na(customerdata$margin_gross_pow_ele)] <- 
  median(customerdata$margin_gross_pow_ele, na.rm = T)

customerdata$margin_net_pow_ele[is.na(customerdata$margin_net_pow_ele)] <- 
  median(customerdata$margin_net_pow_ele, na.rm = T)

customerdata$net_margin[is.na(customerdata$net_margin)] <- 
  median(customerdata$net_margin, na.rm = T)

customerdata$pow_max[is.na(customerdata$pow_max)] <- 
  median(customerdata$pow_max, na.rm = T)

# Missing Value treatment complete.

# Removing Outliers

q <- (quantile(customerdata$cons_12m, 0.99))
q1 <- (quantile(customerdata$cons_12m, 0.01))
q2 <- (quantile(customerdata$forecast_meter_rent_12m, 0.99))
q3 <- (quantile(customerdata$forecast_meter_rent_12m, 0.01))
q4 <- (quantile(customerdata$net_margin, 0.99))
q5 <- (quantile(customerdata$net_margin, 0.01))
q6 <- (quantile(customerdata$margin_gross_pow_ele, 0.99))
q7 <- (quantile(customerdata$margin_gross_pow_ele, 0.01))
q8 <- (quantile(customerdata$cons_gas_12m, 0.99))
q9 <- (quantile(customerdata$cons_gas_12m, 0.01))
q10 <- (quantile(customerdata$forecast_price_pow_p1, 0.99))
q11 <- (quantile(customerdata$forecast_price_pow_p1, 0.01))
q12 <- (quantile(customerdata$forecast_price_energy_p1, 0.99))
q13 <- (quantile(customerdata$forecast_price_energy_p1, 0.01))
q14 <- (quantile(customerdata$forecast_price_energy_p2, 0.99))
q15 <- (quantile(customerdata$forecast_price_energy_p2, 0.01))
q16 <- (quantile(customerdata$forecast_discount_energy, 0.99))
q17 <- (quantile(customerdata$forecast_discount_energy, 0.01))
q18 <- (quantile(customerdata$cons_last_month, 0.99))
q19 <- (quantile(customerdata$cons_last_month, 0.01))
q20 <- (quantile(customerdata$forecast_cons_12m, 0.99))
q21 <- (quantile(customerdata$forecast_cons_12m, 0.01))
q22 <- (quantile(customerdata$forecast_cons_year, 0.99))
q23 <- (quantile(customerdata$forecast_cons_year, 0.01))
q24 <- (quantile(customerdata$margin_net_pow_ele, 0.99))
q25 <- (quantile(customerdata$margin_net_pow_ele, 0.01))
q26 <- (quantile(customerdata$pow_max, 0.99))
q27 <- (quantile(customerdata$pow_max, 0.01))
q28 <- (quantile(customerdata$days_since_activ, 0.997))
q29 <- (quantile(customerdata$days_since_activ, 0.003))
q30 <- (quantile(customerdata$days_since_renewal, 0.997))
q31 <- (quantile(customerdata$days_since_renewal, 0.003))
q32 <- (quantile(customerdata$days_since_modif, 0.997))
q33 <- (quantile(customerdata$days_since_modif, 0.003))

customerdata <- subset(customerdata, customerdata$cons_12m<=q)
customerdata <- subset(customerdata, customerdata$cons_12m>=q1)
customerdata <- subset(customerdata, customerdata$forecast_meter_rent_12m<=q2)
customerdata <- subset(customerdata, customerdata$forecast_meter_rent_12m>=q3)
customerdata <- subset(customerdata, customerdata$net_margin<=q4)
customerdata <- subset(customerdata, customerdata$net_margin>=q5)
customerdata <- subset(customerdata, customerdata$margin_gross_pow_ele<=q6)
customerdata <- subset(customerdata, customerdata$margin_gross_pow_ele>=q7)
customerdata <- subset(customerdata, customerdata$cons_gas_12m<=q8)
customerdata <- subset(customerdata, customerdata$cons_gas_12m>=q9)
customerdata <- subset(customerdata, customerdata$forecast_price_pow_p1<=q10)
customerdata <- subset(customerdata, customerdata$forecast_price_pow_p1>=q11)
customerdata <- subset(customerdata, customerdata$forecast_price_energy_p1<=q12)
customerdata <- subset(customerdata, customerdata$forecast_price_energy_p1>=q13)
customerdata <- subset(customerdata, customerdata$forecast_price_energy_p2<=q14)
customerdata <- subset(customerdata, customerdata$forecast_price_energy_p2>=q15)
customerdata <- subset(customerdata, customerdata$forecast_discount_energy<=q16)
customerdata <- subset(customerdata, customerdata$forecast_discount_energy>=q17)
customerdata <- subset(customerdata, customerdata$cons_last_month<=q18)
customerdata <- subset(customerdata, customerdata$cons_last_month>=q19)
customerdata <- subset(customerdata, customerdata$forecast_cons_12m<=q20)
customerdata <- subset(customerdata, customerdata$forecast_cons_12m>=q21)
customerdata <- subset(customerdata, customerdata$forecast_cons_year<=q22)
customerdata <- subset(customerdata, customerdata$forecast_cons_year>=q23)
customerdata <- subset(customerdata, customerdata$margin_net_pow_ele<=q24)
customerdata <- subset(customerdata, customerdata$margin_net_pow_ele>=q25)
customerdata <- subset(customerdata, customerdata$pow_max<=q26)
customerdata <- subset(customerdata, customerdata$pow_max>=q27)
customerdata <- subset(customerdata, customerdata$days_since_activ<=q28)
customerdata <- subset(customerdata, customerdata$days_since_activ>=q29)
customerdata <- subset(customerdata, customerdata$days_since_renewal<=q30)
customerdata <- subset(customerdata, customerdata$days_since_renewal>=q31)
customerdata <- subset(customerdata, customerdata$days_since_modif<=q32)
customerdata <- subset(customerdata, customerdata$days_since_modif>=q33)

nrow(customerdata)



# DATA EXPLORATION
# Plotting the categorical variables

library(RColorBrewer)
darkcols <- c("darkgoldenrod1", "lightcoral", "steelblue3",  "olivedrab3", "mediumpurple1", "slategray3", "sienna2")


p1 <- ggplot(customerdata, aes(x=channel_sales)) + ggtitle("Distribution of Sales Channel") + xlab("Channel") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill="dark green") + ylab("Percentage (%)") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)

p2 <- ggplot(customerdata, aes(x=origin_up)) + ggtitle("Distribution of Electricity Campaign subscribed by Customers") + xlab("Electricity Campaign Code") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill="dark green") + ylab("Percentage (%)") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)

grid.arrange(p1, p2, ncol=2)

p3 <- ggplot(customerdata, aes(x=churn_plot)) + ggtitle("Distribution of customer churn by March 2016") + xlab("Churned or not") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.4, fill="dark green") + ylab("Percentage (%)") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)


p4 <- ggplot(customerdata, aes(x=has_gas)) + ggtitle("Distribution of customers who have subscribed to gas") + xlab("Has gas or not") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill="dark green") + ylab("Percentage (%)") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)

grid.arrange(p3, p4, ncol=2)

p5 <- ggplot(customerdata, aes(x=nb_prod_act)) + ggtitle("Number of products") + xlab("Number of products") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill="dark green") + ylab("Percentage (%)") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)


p6 <- ggplot(customerdata, aes(x=num_years_antig)) + ggtitle("Antiquity of client (Number of years)") + xlab("Number of years") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, fill="dark green") + ylab("Percentage (%)") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)

grid.arrange(p5, p6, ncol=2)

p7 <- ggplot(customerdata, aes(churn_plot, fill=origin_up)) + ggtitle("Distribution of Churn and Campaign Code") + 
  xlab("Churned by March 2016 or not") + geom_bar(width = 0.5) + ylab("Number of people") + 
  labs(caption = "(Data for SME customers as of January 2016)", fill = "Campaign code") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)


p8 <- ggplot(customerdata, aes(churn_plot, fill=channel_sales)) + ggtitle("Distribution of Churn and Channel Sales Code") + 
  xlab("Churned by March 2016 or not") + geom_bar(width = 0.5) + ylab("Number of people") + 
  labs(caption = "(Data for SME customers as of January 2016)", fill = "Sales channel code") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)


grid.arrange(p7, p8, ncol=2)

p9 <- ggplot(customerdata, aes(churn_plot, fill=has_gas)) + ggtitle("Distribution of Churn and whether the Customer has subscribed for Gas") + 
  xlab("Churned by March 2016 or not") + geom_bar(width = 0.5) + ylab("Number of people") + 
  labs(caption = "(Data for SME customers as of January 2016)", fill = "Subscribed for Gas or not") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)




customerdata$antig_bucket <- cut(customerdata$num_years_antig, c(0,3,6,9,12,20))
p10 <- ggplot(customerdata, aes(churn_plot, fill=antig_bucket)) + ggtitle("Distribution of Churn and Antiquity of Customer") + 
  xlab("Churned by March 2016 or not") + geom_bar(width = 0.5) + ylab("Number of people") + 
  labs(caption = "(Data for SME customers as of January 2016)", fill = "Antiquity of client (in years)") + theme_minimal() + 
  scale_fill_manual(labels = c("1-3", "4-6", "7-9", "10-12", ">12"),values=alpha(c("indianred1", "springgreen3", 
                                                                                   "deepskyblue2","royalblue2","deeppink1"),.9))



p11 <- ggplot(customerdata, aes(churn_plot, days_since_modif)) + ggtitle("Distribution of Churn and Days since Last Modification") + 
  xlab("Churned by March 2016 or not") + geom_boxplot(fill=c("lightcoral","slategray3")) + ylab("Days since Last Modification") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)


p12 <- ggplot(customerdata, aes(churn_plot, days_since_activ)) + ggtitle("Distribution of Churn and Days since active") + 
  xlab("Churned by March 2016 or not") + geom_boxplot(fill=c("lightcoral","slategray3")) + ylab("Days since active") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)


p13 <- ggplot(customerdata, aes(churn_plot, days_since_renewal)) + ggtitle("Distribution of Churn and Days since Renewal") + 
  xlab("Churned by March 2016 or not") + geom_boxplot(fill=c("lightcoral","slategray3")) + ylab("Days since renewal") + 
  labs(caption = "(Data for SME customers as of January 2016)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values=darkcols)

grid.arrange(p10, p12, ncol=2)
grid.arrange(p11, p13, ncol=2)

customerdata$antig_bucket <- NULL
customerdata$churn_plot <- NULL


# Removing unnecessary variables
customerdata$id <- NULL
customerdata$date_activ <- NULL
customerdata$date_end <- NULL
customerdata$date_modif_prod <- NULL
customerdata$date_renewal <- NULL


########### DATA CLEANING AND PRE-PROCESSING ENDS ###########

########### MODELING - MACHINE LEARNING IMPLEMENTATION  ########### 

# Preparing data for modeling
customerdata_model <- customerdata

# Converting categorical features to factors
customerdata_model$channel_sales <- as.factor(customerdata_model$channel_sales)
customerdata_model$origin_up <- as.factor(customerdata_model$origin_up)
customerdata_model$activity_new <- as.factor(customerdata_model$activity_new)
customerdata_model$churn <- as.factor(ifelse(customerdata_model$churn == 1,'yes','no'))

# Partitioning data into Training and Test
index<- createDataPartition(customerdata_model$churn,p=0.75,list=FALSE)
set.seed(2330)
ctrain <- customerdata_model[index,]
ctest <- customerdata_model[-index,]
nrow(ctrain) 
nrow(ctest) 


# Oversampling using SMOTE to make the churn levels have a better proportion
customerdata_smote=SMOTE(churn~., data=ctrain, k=10, perc.over=250, perc.under = 350)
customerdata_smote$churn=as.factor(customerdata_smote$churn)
table(customerdata_smote$churn)
# no    yes 
# 7392 3168


# Building first Logistic Reg Model
LogModel1<- glm(churn ~ .,family = binomial(logit),data = customerdata_smote)

summary(LogModel1) # AIC: 12212

prob <- predict(LogModel1,newdata=ctest,type='response')
fitted.results <- ifelse(prob > 0.5,1,0)
misClasificError <- mean(fitted.results != ctest$churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))
# Logistic Regression Accuracy 0.879592990390051

print("Confusion Matrix for Logistic Regression")
table(ctest$churn, fitted.results > 0.5)

#   FALSE TRUE
# no   3054  132
# yes   317   35

# Precision = 20.9%
# Recall = 9.9%
# F1 score: 13.44%

# Odds ratio
exp(cbind(OR=coef(LogModel1), confint(LogModel1)))

# Building second Logistic Reg Model
LogModel2 <- stepAIC(LogModel1)
summary(LogModel2) # AIC: 12209

prob <- predict(LogModel2,newdata=ctest,type='response')
fitted.results <- ifelse(prob > 0.5,1,0)
misClasificError <- mean(fitted.results != ctest$churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))
# Logistic Regression Accuracy 0.878745053702657

print("Confusion Matrix for Logistic Regression")
table(ctest$churn, fitted.results > 0.5)

#     FALSE TRUE
# no   3055  131
# yes   317   35

# Precision = 21.08%
# Recall = 9.9%
# F1 score: 13.47%


# Building third Logistic Reg Model (k fold -Cross Validation) -- to be used in Ensemble
train_control = trainControl(method="repeatedcv", number=10)
LogModel3 <- caret::train(churn ~., data= customerdata_smote,method='glm',
                          trControl=train_control)
summary(LogModel3)
ctest$pred_lr <- predict(LogModel3, ctest)
caret::confusionMatrix(ctest$pred_lr, ctest$churn)

#   FALSE TRUE
# no   3054  132
# yes   317   35

# Precision = 20.9%
# Recall = 9.9%
# F1 score: 13.44%


# Random Forest Initial Model

rfModel <- randomForest(churn ~., data = customerdata_smote)
print(rfModel)

pred_rf <- predict(rfModel, ctest)
caret::confusionMatrix(pred_rf, ctest$churn)

#             Reference
# Prediction    0    1
#       0     3096  287
#       1      90   65

# Precision of 41.9%
# Recall of 18.4%
# F1 score: 25.57%

plot(rfModel)

# Random Forest model with Cross Validation
control <- trainControl(method="repeatedcv", number=10)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
x <- 18 # number of features 
mtry <- sqrt(x)
rfModel_new <- caret::train(churn~., data=customerdata_smote, method="rf", metric=metric, trControl=control)
print(rfModel_new)

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 12.

plot(rfModel_new)

ctest$pred_rf <- predict(rfModel_new, ctest)
caret::confusionMatrix(ctest$pred_rf, ctest$churn)

#             Reference
# Prediction    no    yes
#        no   3115  292
#        yes    71   60

# Precision of 45.8%
# Recall of 17%
# F1 score: 24.8%

varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


# KNN Implementation
train_control = trainControl(method="repeatedcv", number=10)
knnModel <- caret::train(churn ~., data= customerdata_smote,
                 method='knn',trControl=train_control)

ctest$pred_knn <- predict(knnModel, ctest)
caret::confusionMatrix(ctest$pred_knn, ctest$churn)

#             Reference
# Prediction   no  yes
#       no    2682  273
#      yes    504   79

# Precision of 13.5%
# Recall of 22.44%
# F1 score: 16.86%


# Ensemble Model (Logistic Regression + Random Forest + KNN)

#Predicting the probabilities for the goal from the best models across all three types

ctest$pred_rf_prob <- predict(rfModel_new,ctest,type='prob')
ctest$pred_knn_prob <- predict(knnModel,ctest,type='prob')
ctest$pred_lr_prob <- predict(LogModel3,ctest,type='prob')


#Taking average of predictions
ctest$pred_avg<-(ctest$pred_rf_prob$yes+ctest$pred_knn_prob$yes+ctest$pred_lr_prob$yes)/3
ctest$pred_avg.1<-ctest$pred_avg

#Splitting into binary classes at 0.5
ctest$pred_avg<-as.factor(ifelse(ctest$pred_avg>0.5,'yes','no'))
table(ctest$pred_avg)
caret::confusionMatrix(ctest$pred_avg, ctest$churn)

#               Reference
#   Prediction   no  yes
#       no      3126  317
#       yes     60   35

# Precision of 36.8%
# Recall of 9.9%
# F1 score: 15.6%

#Taking weighted average of predictions (0.5 for Random Forest since it's shown the best performance)

ctest$pred_weighted_avg<-(ctest$pred_rf_prob$yes*0.5)+(ctest$pred_knn_prob$yes*0.25)+(ctest$pred_lr_prob$yes*0.25)
ctest$pred_weighted_avg.1<-ctest$pred_weighted_avg

#Splitting into binary classes at 0.5
ctest$pred_weighted_avg<-as.factor(ifelse(ctest$pred_weighted_avg>0.5,'yes','no'))
table(ctest$pred_weighted_avg)
caret::confusionMatrix(ctest$pred_weighted_avg, ctest$churn)

#               Reference
#   Prediction   no  yes
#          no  3149  309
#          yes   37   43
                                          
# Accuracy : 90.22% 
# Precision of 53.7%
# Recall of 12.2%
# F1 score: 19.88%

# Converting output variables to numeric to calculate AUC
ctest$churn <- as.numeric(ifelse(ctest$churn == 'yes',1,0))
ctest$pred_rf <- as.numeric(ifelse(ctest$pred_rf == 'yes',1,0))
ctest$pred_knn <- as.numeric(ifelse(ctest$pred_knn == 'yes',1,0))
ctest$pred_lr <- as.numeric(ifelse(ctest$pred_lr == 'yes',1,0))
ctest$pred_avg.1 <- as.numeric(ifelse(ctest$pred_avg.1 == 'yes',1,0))
ctest$pred_weighted_avg.1 <- as.numeric(ifelse(ctest$pred_weighted_avg.1 == 'yes',1,0))

# AUC of individual algorithms and ensembled models
rf_auc<-auc(ctest$churn,ctest$pred_rf)
print(paste('AUC of RF',rf_auc))
# "AUC of RF 0.593979163099926"

knn_auc<-auc(ctest$churn,ctest$pred_knn)
print(paste('AUC of KNN',knn_auc))
# "AUC of KNN 0.51684304200194"

glm_auc<-auc(ctest$churn,ctest$pred_lr)
print(paste('AUC of GLM',glm_auc))
# "AUC of GLM 0.531205415739314"

ensemble_auc<-auc(ctest$churn,ctest$pred_avg.1)
print(paste('AUC of ensembled model',ensemble_auc))
# "AUC of ensembled model 0.5"

ensemble_auc.weighted_av<-auc(ctest$churn,ctest$pred_weighted_avg.1)
print(paste('AUC of ensembled model',ensemble_auc.weighted_av))
# "AUC of ensembled model 0.5"


########### MODELING - MACHINE LEARNING IMPLEMENTATION DONE  ########### 

