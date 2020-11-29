# Getting and setting the appropriate working directory
getwd()
setwd("/Users/mihikagupta/Desktop/SEM_2/DADM/DADM_FinalProject")

# Loading relevant libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(treemapify))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(plotrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(tree))
suppressPackageStartupMessages(library(rattle))
library("car")
library(MASS)
library(cluster)
library(MLmetrics)
library(vegan)
library(party)
library(lmtest)
library(stats)
library(corrplot)
library(pROC)
library(plyr)

# Loading the dataset
data<-read.csv("attrition.csv")

# Viewing inistial data
head(data)

# Checking Column-names and attributes
names(data)

# Checking the Dependent variable, and prediction of which is our main objective of the project
data$Attrition
# we observe here that our dependent variable is a binary factor variable having levels "Yes" and "No"


################################## Getting to know our data firstly #######################################

# Shape and size
nrow(data)
ncol(data)

# Checking the data structure we have
str(data)
summary(data)

# Checking for null attributes
is.null(data) 
# we have no nulls and therefore can easily move ahead

# Using an insightful summary with skim and table
data %>% glimpse()

# looking at type of data
class(data)

# looking at datatypes of each attribute
t(sapply(data, class)) 

# dropping irrelevant data columns
data = data[, !(names(data)%in% c('Over18','EmployeeNumber', 'EmployeeCount', 'StandardHours'))]
# Checking new data again
head(data)
View(data)
nrow(data)
ncol(data)

################################################### EDA ################################################################################

# checking correlation between attributes
num_cols <- unlist(lapply(data, is.numeric))         # Identify numeric columns
num_cols
data_num <- data[ , num_cols]                        # Subset numeric columns of data
data_num  
M <- cor(data_num)
M
corrplot(M, method="circle")
# here we observe that there are some strong positive correlation between:-
# 1. Age and Total Working years
# 2. Job Level and Job Satisfaction
# 3. Job Level and Total Working years
# 4. Job Level and Monthly Income
# 5. Total Working years and Monthly Income
# 6. Performance Rating and Percent Salary Hike

# Changing the dependent variable to factor
data$Attrition<-as.factor(data$Attrition)
class(data$Attrition)
# checking current level names
levels(data$Attrition)
# changing the factor names to 0 and 1 
data$Attrition<-revalue(data$Attrition, c("No"="0", "Yes"="1"))
View(data)

# some variables are categorical and should have datatype as factor, but they are labeled integers
# now we fix this
factors <- c("BusinessTravel", "Department", "DistanceFromHome",
               "Education", "EducationField", "EnvironmentSatisfaction", "Gender",
               "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus",
               "OverTime", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance")

data[factors] = lapply(data[factors], factor)
t(sapply(data, class)) # successfully changed data types

################################################### Visualization #######################################################
# Imbalance in dataset
options(repr.plot.width=8, repr.plot.height=4)
ggplot(data, aes(Attrition,fill = Attrition))+geom_histogram(stat="count")+
  labs(title="Employee Attrition (Amount)", x="Employee Attrition",y="Amount")

################################################### Visualization #######################################################
# Checking data normality for monthly income vs attrition
qplot(TotalWorkingYears, MonthlyIncome, data = data)+ geom_smooth(method = "lm", se=F)+ facet_grid(. ~ Attrition)

################################################### Visualization #######################################################
# Is overtime a contributor for higher attrition levels
# Overtime vs Attrition
ggplot(data, aes(OverTime,fill = Attrition))+ geom_histogram(stat="count")
# here we observe that when employee was working overtime, the attrition levels were also higher

################################################### Visualization #######################################################
# Marital Status vs Attrition
ggplot(data, aes(MaritalStatus,fill = Attrition))+geom_histogram(stat="count")
# single people have more tendency to be subject to attrition
  
################################################### Visualization #######################################################
###JobRole vs Attrition
ggplot(data, aes(JobRole,fill = Attrition))+geom_histogram(stat="count")
# here we see some job roles were more likely to face attrition than others

################################################### Visualization #######################################################
# Gender vs Attrition
ggplot(data, aes(Gender,fill = Attrition))+geom_histogram(stat="count")
# Gender does not seem to be a significant contributor towards attrition

################################################### Visualization #######################################################
# Education Field vs Attrition
ggplot(data, aes(EducationField,fill = Attrition))+geom_histogram(stat="count")

################################################### Visualization #######################################################
# Department vs Attrition
ggplot(data, aes(Department,fill = Attrition))+geom_histogram(stat="count")
# we dont see any significant trend here

################################################### Visualization #######################################################
# Education vs Attrition
ggplot(data, aes(Education,fill = Attrition))+geom_histogram(stat="count")

################################################### Visualization #######################################################
# Business Travel vs Attrition
ggplot(data, aes(BusinessTravel,fill = Attrition))+geom_histogram(stat="count")
# when travel freq is less, this case is less prone to attrition

################################################### Visualization #######################################################

# LOOKING AT GENDER, AGE DISTRIBUTION

# Average age by gender
average_ageGender <- data %>% group_by(Gender) %>% summarize(avg=mean(Age))
average_ageGender 

# Age distribution by gender
gender.dist <- data %>% filter(Gender == 'Male' | Gender== "Female") %>% 
  filter(!is.na(Age)) %>% group_by(Gender) %>% 
  ggplot(aes(x=Age)) + geom_density(aes(fill=Gender), alpha=0.8, show.legend=FALSE) + facet_wrap(~Gender) + theme_minimal() + 
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) + labs(title="Age Distribution") 

overall.dist <- data %>% filter(!is.na(Age)) %>% 
  ggplot(data=data, mapping=aes(x=Age)) + geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) +  theme_minimal() + labs(x="Overall Age")

plot_grid(gender.dist, overall.dist, nrow=2)

################################################### Visualization #######################################################

# Monthly Income by gender, we observe almost equal payscales irrespective of the gender, females being slightly higher
ggplot(data, aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + geom_boxplot()+labs(title="Are there any Gender Disparities in Income?")

################################################### Visualization #######################################################

# Understanding generational behaviour, let's see if young people worked in more companies than the older generation
# This might prove that the millenial's tend to be more picky with regards to jobs than the older generation.

# First we must create categorical variables based on Age
data$Generation <- ifelse(data$Age<37,"Millenials",
                        ifelse(data$Age>=38 & data$Age<54,"Generation X",
                               ifelse(data$Age>=54 & data$Age<73,"Boomers","Silent"
                               )))

# Let's see the distribution by generation now
generation.dist <- data %>% 
  ggplot() + geom_boxplot(aes(x=reorder(Generation, NumCompaniesWorked, FUN=median), 
                              y=NumCompaniesWorked, fill=Generation)) + facet_wrap(~Attrition) + labs(title="Knowing Past Generations",x="Generation", y="Number of Companies Previously Worked")
generation.dist
# here we observe by the general trend that irrespective of any generation, when people worked in more number of companies,
# attrition was more likely to happen.

################################################### Visualization #######################################################
# Overtime,Age, and Marital Status VS Attrition
ggplot(data, aes(OverTime, Age)) +  
  facet_grid(.~MaritalStatus) +
  geom_jitter(aes(color = Attrition),alpha = 0.4) +  
  ggtitle("x=Overtime, y= Age, z = MaritalStatus , t = Attrition") 

################################################# LOGISTIC REGRESSION ######################################################

# Since our dependent variable is a binary categorical variable, we use logistic regression method 
data<-data[,-32] # removing the Generation created attribute

# Splitting data,set the seed to make your partition reproducible
set.seed(123)

# 75% of the sample size will be train and 25% is test
smp_size <- floor(0.75 * nrow(data))
train_size <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_size, ]
test <- data[-train_size, ]

# Applying logistic regression
model_log<- glm(Attrition ~., family = binomial(link = 'logit'), data =train)

# checking summary
summary(model_log)

# predictions using first initial model
pred<-predict(model_log,test)
pred<-ifelse(pred>0.5,1,0)
pred

# Checking the confusion matrix
table(pred,test$Attrition)
confusionMatrix(test$Attrition,pred)

# Reducing or pruning the model for better results, lets check the p values
summary(model_log)

# After checking the p-values for all variables, we observe that few are more significant than others, we perform logistic regression with only these variables
# ['Age','BusinessTravel', 'DistanceFromHome', 'EnvironmentSatisfaction', 'JobInvolvement','JobLevel', 'JobSatisfaction', 'MaritalStatus', 'NumCompaniesWorked', 'OverTime', 'RelationshipSatisfaction','StockOptionLevel' ,'TotalWorkingYears', 'TrainingTimesLastYear', 'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion']
selection<-c('Attrition','Age','BusinessTravel', 'DistanceFromHome', 'EnvironmentSatisfaction', 'JobInvolvement','JobLevel', 'JobSatisfaction', 'MaritalStatus', 'NumCompaniesWorked', 'OverTime', 'RelationshipSatisfaction','StockOptionLevel' ,'TotalWorkingYears', 'TrainingTimesLastYear', 'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion')

data1= data[,selection]

# 75% of the sample size will be train and 25% is test
smp_size1 <- floor(0.75 * nrow(data1))
train_size1 <- sample(seq_len(nrow(data1)), size = smp_size1)
train1 <- data1[train_size1, ]
test1 <- data1[-train_size1, ]

# Applying new model
model_select<- glm(Attrition ~., family = binomial(link = 'logit'), data = train1)

# checking summary
summary(model_select)

# predictions using first initial model
pred1<-predict(model_select,test1)
pred1<-ifelse(pred1>0.5,1,0)
pred1

# Checking the confusion matrix
table(pred1,test1$Attrition)
## Checking the new confusion matrix shows higher accuracy levels than original model

# Comapring the original and select model using ANOVA
anova(model_log,model_select)
# the results of anova also show that the select model performed better

########### Now lets apply Stepwise regression on this model ###########

step(model_select)






