rm(list=ls()) # remove all existing objects in the environment
gc() # garbage collection

library(dplyr)
library(corrplot)
library(fastDummies)

##......Read The Data......##
setwd('C:/Users/Aniket/Desktop/New folder')
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


write.csv(data,'C:/Users/Aniket/Desktop/New folder/Clean_Data.csv', row.names = FALSE)

c_data <- read.csv('C:/Users/Aniket/Desktop/New folder/Clean_Data.csv', header = T)
str(c_data)

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


#partition (80 %, 20%)
set.seed(1234)
ind <- sample(2, nrow(c_data), replace = T, prob = c(0.8,0.2))
train <- c_data[ind==1, ]
test <- c_data[ind==2, ]

train

# logistic regr model

c_model <- glm(y ~ ., data = train, family = 'binomial')
summary(c_model)

#only significant variables

c_model1 <- glm(y ~ balance + job_retired + day + month + previous +
                  job_student + job_unemployed + marital_divorced + marital_single + education_primary + 
                  education_tertiary + housing_no + loan_yes + pday_bins_0.5.Months + 
                  pday_bins_10.15.Months + pday_bins_15.20.Months, data = train, family = 'binomial')
summary(c_model1)

# predictions

p1 <- predict(c_model1, train, type = 'response')
head(p1)

#miscalssification 
pred1 <- ifelse(p1>0.5, 1, 0)

confusion_matrix1 <- table(Predicted = pred1, Actual = train$y)

misclassfication_error_train <- 1 - sum(diag(confusion_matrix1))/sum(confusion_matrix1)

#Misclassification Error on Training data is about 11%

# Model on test data
p2 <- predict(c_model1, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)

confusion_matrix2 <- table(Predicted = pred2, Actual = test$y)
misclassfication_error_test <- 1 - sum(diag(confusion_matrix2))/sum(confusion_matrix2)

#Misclassification Error on Test data is still about 11%

#goodness-of-fit test
with(c_model1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))


#p-value is infinitesimally small, that is very close to zero. So the cofidence level is high that
#the model is statiscally significant.