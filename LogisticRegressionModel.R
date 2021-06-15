rm(list=ls()) # remove all existing objects in the environment
gc() # garbage collection

library(dplyr)
library(corrplot)
library(fastDummies)
library(MASS)

##......Read The Data......##
setwd('C:/Users/abc/New folder')
data = read.csv('bank-full.csv', head=T, stringsAsFactors=F, na.strings='')

View(data)
dim(data)
summary(data)
names(data)
str(data)
head(data)

##########################################################################
###################### DATA CLEANING #####################################
##########################################################################

# STEP 1: Check missing values

#---------Finding and Handling N/A values-----------#
colSums(is.na((data))) 
colSums(data == 'unknown')

data[data == "unknown"] <- NA
colSums(is.na((data)))

missing <- is.na(data)
missing
cmiss <- colMeans(missing)
cmiss
rmiss <- rowMeans(missing)
rmiss

data = data[, cmiss < .2]   #..dropping the comlumns with greater than 20% missing values i.e contact and poutcome..#
data = data[rmiss <.2,]   #..dropping rows that has missing values greater than 20%..#

#  We are left with 2145 missing values i.e 4% of the data 
#  As the proportion of missing data is very less, we decide to omit it
data <- na.omit(data)

#  Since it is mentioned in the UCI data description that the column 'duration' 
#  is included for benchmark purposes and should be discarded if the intention 
#  is to have a realistic predictive model.
#  So, we delete the Duration column.

data$duration = NULL
dim(data)
#----(END)Finding and Handling Missing Values(END)-----#



# STEP 2 - Distribution of Continuous/Categorical Variables

par(mfrow=c(1, 2)) # c(1, 2) give a graphing panel with two plots

## check the distribution of continuous variables #

#age
hist(data$age) # histogram of the column "Price" of the data "data2"
boxplot(data$age) # boxplot
summary(data$age)

#balance
hist(data$balance) # histogram of the column "Price" of the data "data2"
boxplot(data$balance) # boxplot
summary(data$balance)

#campaign
hist(data$campaign) # histogram of the column "Price" of the data "data2"
boxplot(data$campaign) # boxplot
summary(data$campaign)

#pdays
hist(data$pdays) # histogram of the column "Price" of the data "data2"
boxplot(data$pdays) # boxplot
summary(data$pdays)

#previous
hist(data$previous) # histogram of the column "Price" of the data "data2"
boxplot(data$previous) # boxplot
summary(data$previous)

## check the frequency table of categorical variables
table(data$job)
table(data$marital)
table(data$education)
table(data$default)
table(data$housing)
table(data$loan)
table(data$day)
table(data$month)



# STEP 3 - Delete Outliers for Numeric Variables

#.....Handling Extreme Outliers.....#

#------Outliers in Age--------#
boxplot(data$age, horizontal = TRUE)
boxplot.stats(data$age)
data<- subset(data, age<=90)
summary(data$age)
boxplot(data$age, horizontal = TRUE)
#---(END)Outliers in Age(END)---#

#------Outliers in Balance--------#
boxplot(data$balance, horizontal = TRUE)
boxplot.stats(data$balance)
data<- subset(data, balance<=28000)
data<- subset(data, -2200<=balance)
summary(data$balance)
boxplot(data$balance, horizontal = TRUE)
#---(END)Outliers in Balance(END)---#

#------Outliers in Day--------#
boxplot(data$day, horizontal = TRUE)
boxplot.stats(data$day)
summary(data$day)
boxplot(data$day, horizontal = TRUE)
#---(END)Outliers in Day(END)---#

#------Outliers in Campaign--------#
boxplot(data$campaign, horizontal = TRUE)
boxplot.stats(data$campaign)
data<- subset(data, campaign<=40)
summary(data$campaign)
boxplot(data$campaign, horizontal = TRUE)
#---(END)Outliers in Campaign(END)---#

#------Outliers in pdays--------#
boxplot(data$pdays, horizontal = TRUE)
boxplot.stats(data$pdays)
data<- subset(data, pdays<=575)
summary(data$pdays)
boxplot(data$pdays, horizontal = TRUE)
#---(END)Outliers in pdays(END)---#

#------Outliers in previous--------#
boxplot(data$previous, horizontal = TRUE)
boxplot.stats(data$previous)
data<- subset(data, previous<=30)
summary(data$previous)
boxplot(data$previous, horizontal = TRUE)
#---(END)Outliers in previous(END)---#

#.....(END)Handling Extreme Outliers(END).....#


#-----Binning pdays Column-----#
pdaybin <- function(pdays){
  if(pdays==-1){
    return("Never Contacted")
  }else if (pdays >= 0 & pdays <= 150){
    return('0-5 Months')
  }else if(pdays > 150 & pdays <= 300){
    return('5-10 Months')
  }else if (pdays > 300 & pdays <= 450){
    return('10-15 Months')
  }else if (pdays > 450 & pdays <=600){
    return('15-20 Months')
  }
}

data$pday_bins <- sapply(data$pdays,pdaybin)
data$pday_bins <- as.factor(data$pday_bins)

data$pdays <- NULL
#-----(END)Binning pdays Column(END)-----#



# STEP 4 - Creating Dummy Variables for Categorical Variable

data = dummy_columns(data, select_columns = 'job', remove_most_frequent_dummy = T)
colnames(data)
data$job = NULL

data = dummy_columns(data, select_columns = 'marital', remove_most_frequent_dummy = T)
colnames(data)
data$marital = NULL

data = dummy_columns(data, select_columns = 'education', remove_most_frequent_dummy = T)
colnames(data)
data$education = NULL

data = dummy_columns(data, select_columns = 'default', remove_most_frequent_dummy = T)
colnames(data)
data$default = NULL

data = dummy_columns(data, select_columns = 'housing', remove_most_frequent_dummy = T)
colnames(data)
data$housing = NULL

data = dummy_columns(data, select_columns = 'loan', remove_most_frequent_dummy = T)
colnames(data)
data$loan = NULL

data$month = factor(data$month,
                       levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),
                       labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12))
data$month <- as.integer(data$month)

data = dummy_columns(data, select_columns = 'pday_bins', remove_most_frequent_dummy = T)
colnames(data)
data$pday_bins = NULL

#--changing the output variable to 0's and 1's--#
data$y[data$y == "no"] <- 0
data$y[data$y == "yes"] <- 1
data$y <- as.integer(data$y)


str(data)



# STEP 5 - Check Correlation:

data %>% cor() %>% round(2) %>% View()

corrplot(cor(data), method="number")

# #There is high correlation (.95) between pdays and previous. 
# #Hence we will remove "previous" column
# data$previous = NULL
# data %>% cor() %>% round(2) %>% View()

#---Correlation between Numeric Variables---#
data_num <- subset(data, select = c(age, balance, day,campaign,y))
cor_cat=cor(data_num)
corrplot(cor_cat, method = "color", type="lower",bg='red', addCoef.col = 'red', number.font = 6)


##########################################################################
################ (END) DATA CLEANING (END) ###############################
##########################################################################


write.csv(data,'C:/Users/abc/Clean_Data.csv', row.names = FALSE)

c_data <- read.csv('C:/Users/abc/Clean_Data.csv', header = T)
str(c_data)

#drop unwanted columns from the data
drop <- c("pday_bins_0.5.Months","pday_bins_10.15.Months","pday_bins_15.20.Months","pday_bins_5.10.Months")
c_data = c_data[,!(names(c_data) %in% drop)]


"
c_data$campaign <- as.factor(c_data$campaign)
c_data$y <- as.factor(c_data$y)
c_data$job_admin. <- as.factor(c_data$job_admin.)
c_data$job_entrepreneur <- as.factor(c_data$job_entrepreneur)
c_data$job_housemaid  <- as.factor(c_data$job_housemaid )
c_data$job_management  <- as.factor(c_data$job_management )
c_data$job_retired <- as.factor(c_data$job_retired)
c_data$job_self.employed <- as.factor(c_data$job_self.employed)
c_data$job_services <- as.factor(c_data$job_services)
c_data$job_student <- as.factor(c_data$job_student)
c_data$job_technician <- as.factor(c_data$job_technician)
c_data$job_unemployed  <- as.factor(c_data$job_unemployed )
c_data$marital_divorced <- as.factor(c_data$marital_divorced)
c_data$marital_single  <- as.factor(c_data$marital_single)
c_data$education_primary <- as.factor(c_data$education_primary)
c_data$education_tertiary <- as.factor(c_data$education_tertiary)
c_data$default_yes <- as.factor(c_data$default_yes)
c_data$housing_no <- as.factor(c_data$housing_no)
c_data$loan_yes  <- as.factor(c_data$loan_yes)
c_data$pday_bins_0.5.Months <- as.factor(c_data$pday_bins_0.5.Months)
c_data$pday_bins_10.15.Months <- as.factor(c_data$pday_bins_10.15.Months)
c_data$pday_bins_15.20.Months <- as.factor(c_data$pday_bins_15.20.Months)
c_data$pday_bins_5.10.Months <- as.factor(c_data$pday_bins_5.10.Months)
c_data$day <- as.factor(c_data$campaign)
c_data$month <- as.factor(c_data$month)
c_data$previous <- as.factor(c_data$previous)

str(c_data)
"

min.model = glm(y ~ 1, data = c_data, family = 'binomial')
max.model = glm(y ~ ., data = c_data, family = 'binomial')
max.formula = formula(max.model)
min.formula = formula(min.model)


get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b + qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb.or, ub.or, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}
get.or(summary(obj_fo))



set.seed(1) # set a seed so that people get the same 60% next time they run the same code
id.train = sample(1:nrow(c_data), nrow(c_data)*.6) # ncol() gives number of columns
id.test = setdiff(1:nrow(c_data), id.train) # setdiff gives the set difference
c_data.train = c_data[id.train,]
c_data.test = c_data[id.test,]

#-----Forward Selection-----

min.model = glm(y ~ 1, data = c_data.train, family = 'binomial')
max.model = glm(y ~ ., data = c_data.train, family = 'binomial')
max.formula = formula(max.model)
min.formula = formula(min.model)

obj_fo = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(obj_fo)

yhat = predict(obj_fo, newdata = c_data.test, type='response')
hist(yhat)

#----CutOFF 0.3 ------
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .3)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----CutOFF 0.4 ------
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .4)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----CutOFF 0.5 ------
dichotomize = function(yhat, cutoff=.1) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .5)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#-----Backward selection-----
min.model = glm(y ~ 1, data = c_data.train, family = 'binomial')
max.model = glm(y ~ ., data = c_data.train, family = 'binomial')
max.formula = formula(max.model)
min.formula = formula(min.model)

obj_bo = step(min.model, direction='backward' , scope=max.formula) # it will print out models in each step
summary(obj_bo)

yhat = predict(obj_bo, newdata = c_data.test, type='response')
hist(yhat)


#----CutOFF 0.3 ------
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .3)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----CutOFF 0.4 ------
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .4)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----CutOFF 0.5 ------
dichotomize = function(yhat, cutoff=.1) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .5)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----STEPWISE SELECTION-----


min.model = glm(y ~ 1, data = c_data.train, family = 'binomial')
max.model = glm(y ~ ., data = c_data.train, family = 'binomial')

max.formula = formula(max.model)
min.formula = formula(min.model)

obj_sw <- step(min.model, direction='both', scope=max.formula) # it will print out models in each step
summary(obj_sw)

yhat = predict(obj_sw, newdata = c_data.test, type='response')
hist(yhat)


#----CutOFF 0.3 ------
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .3)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----CutOFF 0.4 ------
dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .4)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

#----CutOFF 0.5 ------
dichotomize = function(yhat, cutoff=.1) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .5)
err = mean(yhat.class != c_data.test$y) # misclassification error rate
err

table(yhat.class, c_data.test$y)

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(c_data.test$y, yhat.class)
spe(c_data.test$y, yhat.class)

yhat = predict(obj_sw, newdata = c_data.test, type='response')
hist(yhat)
table(yhat.class, c_data.test$y)

hatt = prediction(yhat, c_data.test$y)
roc= performance(hatt, "tpr", "fpr")
plot(roc,
     main="ROC CURVE",
     ylab="Sensitivity",
     xlab="Specificity")

eval = performance(hatt, "auc")
plot = unlist(slot(eval, "y.values"))
plot
plot = round(plot,4)
legend(0.6,0.2,plot,title="AUC")
