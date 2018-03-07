# Logistic regression with Titanic dataset
########## Read the dataset ##########
titanic.train = read.csv(file = 'C:\\Users\\dsharma\\Desktop\\Study\\Titanic\\train.csv',
                         header = TRUE,
                         stringsAsFactors = FALSE)

str(titanic.train)

# Find the missing values - See Automated - Finding NA.R
########## Imputing missing data in Age column ##########
# Impute the missing age data with the average age of each class.
# Finding average age by class
age.by.class = aggregate(titanic.train[, 'Age'], 
                         list(titanic.train$Pclass),
                         mean, 
                         na.rm = TRUE)
colnames(age.by.class) = c("PClass", "AvgAge")

# Replacing the average age
total.rows = nrow(titanic.train)
for (row.num in 1:total.rows) {
  if (is.na(titanic.train[row.num, "Age"])) {
    titanic.train[row.num, "Age"] = age.by.class[age.by.class$PClass == titanic.train[row.num, "Pclass"], ]$AvgAge
  }
}

########## Removing the columns that aren't required ##########
library(dplyr)
titanic.train.new = select(titanic.train, -PassengerId, -Name, -Ticket, -Cabin)

########## Changing the columns to factor ##########
titanic.train.new$Survived = factor(titanic.train.new$Survived)
titanic.train.new$Pclass = factor(titanic.train.new$Pclass)
titanic.train.new$Parch = factor(titanic.train.new$Parch)
titanic.train.new$SibSp = factor(titanic.train.new$SibSp)
titanic.train.new$Sex = factor(titanic.train.new$Sex)
titanic.train.new$Embarked = factor(titanic.train.new$Embarked)

########## Splitting data into train and test set ##########
# Dividing the data into 75% in train set and 25% in test set
sample.size = floor(0.75 * nrow(titanic.train.new))
set.seed(123)
train.ind = sample(seq_len(nrow(titanic.train.new)),size = sample.size)
titanic.train.train = titanic.train.new[train.ind, ]
titanic.train.test = titanic.train.new[-train.ind, ]

########## Creating Model ############
log.model.titanic = glm(formula = Survived ~ . , 
                        family = binomial(link = "logit"),
                        data = titanic.train.train)
summary(log.model.titanic)
# The lower the value of Pr(>|z|) the more important the variable is to the model. The number
# of * in the end of line also indicates the importance of that variable.
########## Predicting from model ##########
fitted.probabilities = predict(object = log.model.titanic, newdata = titanic.train.test, type = 'response')
fitted.results = ifelse(fitted.probabilities > 0.5, 1, 0)
miss.class.error = mean(fitted.results != titanic.train.test$Survived)
accuracy = 1 - miss.class.error
accuracy

########## Confusion Matrix ##########
table(titanic.train.test$Survived, fitted.probabilities > 0.5)
# 
#         FALSE       TRUE
#   0   TN = 128     FP = 17
#   1   FN =  19     TP = 59
############################### ENDS HERE ###############################



######################## ADULT SALARY PROJECT STARTS HERE ########################
adult.sal.input = read.csv(file = "C:\\Users\\dsharma\\Desktop\\R\\Udemy Course\\R-for-Data-Science-and-Machine-Learning\\Training Exercises\\Machine Learning Projects\\CSV files for ML Projects\\adult_sal.csv", header = TRUE, stringsAsFactors = FALSE)
View(adult.sal.input)
# NULL is represented by ? in data. So it is not NULL in the strict sense of R but
# signifies missing data
########## Cleaning Adult Salary Data ########## 
# Removing the index column
library(dplyr)
adult.sal.input = select(adult.sal.input, - X)
# Cleaning Employer Type
table(adult.sal.input$type_employer)
empTypeChange = function(job){
  job = as.character(job)
  job = ifelse(job == 'Never-worked' | job == 'Without-pay', 'Unemployed', job)
  job = ifelse(job == 'Self-emp-inc' | job == 'Self-emp-not-inc', 'Self-emp', job)
  job = ifelse(job == 'State-gov' | job == 'Local-gov', 'SL-gov', job)
  return(job)
}
adult.sal.input$type_employer = sapply(adult.sal.input$type_employer, empTypeChange)
table(adult.sal.input$type_employer)

# Cleaning Marital Status
table(adult.sal.input$marital)
maritalChange = function(status){
  status = as.character(status)
  status = ifelse(status == 'Divorced' | status == 'Separated' | status == 'Widowed', 'Not-Married', status)
  status = ifelse(status == 'Married-AF-spouse' | status == 'Married-civ-spouse' | status == 'Married-spouse-absent', 'Married', status)
  return(status)
}
adult.sal.input$marital = sapply(adult.sal.input$marital, maritalChange)
table(adult.sal.input$marital)

# Cleaning country
table(adult.sal.input$country)
# Grouping by Continents
Asia = c('China','Hong','India','Iran','Cambodia','Japan', 'Laos','Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
North.America = c('Canada','United-States','Puerto-Rico' )
Europe = c('England' ,'France', 'Germany','Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.and.South.America = c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago')
Other = c('South', '?')
group_country = function(country){
  country = as.character(country)
  country = ifelse(country %in% Asia, 'Asia', country)
  country = ifelse(country %in% North.America, 'North.America', country)
  country = ifelse(country %in% Europe, 'Europe', country)
  country = ifelse(country %in% Latin.and.South.America, 'Latin.and.South.America', country)
  country = ifelse(country %in% Other, 'Other', country)
  return(country)
}
adult.sal.input$country = sapply(adult.sal.input$country, group_country)
table(adult.sal.input$country)
# Rename the country column to Region column using dplyr
adult.sal.input = rename(adult.sal.input, region = country)
# Changing the columns to factor
adult.sal.input$type_employer = factor(adult.sal.input$type_employer)
adult.sal.input$marital = factor(adult.sal.input$marital)
adult.sal.input$region = factor(adult.sal.input$region)
adult.sal.input$education = factor(adult.sal.input$education)
adult.sal.input$occupation = factor(adult.sal.input$occupation)
adult.sal.input$relationship = factor(adult.sal.input$relationship)
adult.sal.input$race = factor(adult.sal.input$race)
adult.sal.input$sex = factor(adult.sal.input$sex)
adult.sal.input$income = factor(adult.sal.input$income)
str(adult.sal.input)

# Convert the ? to NA. 
adult.sal.input[adult.sal.input == '?'] = NA

# Refactor the columns so that ? is removed from factor level
adult.sal.input$type_employer = factor(adult.sal.input$type_employer)
adult.sal.input$occupation = factor(adult.sal.input$occupation)

table(adult.sal.input$type_employer)

# Finding which columns have null values
# Calling the function I have defined in a different R Script
source("C:\\Users\\dsharma\\Desktop\\R\\RStudio_Workspaces\\Automated - Finding NA function.R")
missing.data.df = missing.data(adult.sal.input)
missing.data.df

# If the columns that have missing values are factor columns then we may ultimately
# remove the columns
adult.sal.input = na.omit(adult.sal.input)

# Creating the logistic model
# Splitting the data into train and test
sample.size = floor(0.75 * nrow(adult.sal.input))
set.seed(123)
train.ind = sample(seq_len(nrow(adult.sal.input)),size = sample.size)
adult.sal.train = adult.sal.input[train.ind, ]
adult.sal.test = adult.sal.input[-train.ind, ]

adult.sal.log.model = glm(formula = income ~ ., family = binomial(link = 'logit'), data = adult.sal.train)
summary(adult.sal.log.model)
# Using step to identify the features that are significant for the model. 
new.step.model = step(adult.sal.log.model)

# Predicting the income
adult.sal.test$Predicted_income = predict(object = adult.sal.log.model, newdata = adult.sal.test, type = 'response')
# Create confusion Matrix
conf.matrix = table(adult.sal.test$income, adult.sal.test$Predicted_income > 0.5)
true.negative = conf.matrix[1, 1]
false.positive = conf.matrix[1, 2]
false.negative = conf.matrix[2, 1]
true.positive = conf.matrix[2, 2]

accuracy = (true.positive + true.negative) / nrow(adult.sal.test) * 100
recall = true.negative / (true.negative + false.positive) * 100
precision = true.negative / (true.negative + false.negative) * 100
accuracy
recall
precision

######################## ADULT SALARY PROJECT ENDS HERE ########################



# Changing the value of Parch = 6
# titanic.train.test$Parch = ifelse(titanic.train.test$Parch == 6, 0, titanic.train.test$Parch)
# Making the column as factor again
# titanic.train.test$Parch = factor(titanic.train.test$Parch)
# 
# View(titanic.train.test)
# View(titanic.train.train)
# testing = titanic.train.test
# testing$Parch = ifelse(testing$Parch == 6, 0, testing$Parch)
# View(testing)
# ifelse(testing$Parch == 6, 0, testing$Parch)



########## Cleaning the test dataset ##########
# The titatic test set does not have "Survived" column so we can really use it to test our 
# model. 
titanic.test = read.csv(file = 'C:\\Users\\dsharma\\Desktop\\Study\\Titanic\\test.csv',
                        header = TRUE,
                        stringsAsFactors = FALSE)
# It is essential to clean the test data set similar to train dataset so that both the
# datasets have same structure.
# Imputing the age in test dataset
age.by.class.test = aggregate(titanic.test[, 'Age'], 
                         list(titanic.test$Pclass),
                         mean, 
                         na.rm = TRUE)
colnames(age.by.class.test) = c("PClass", "AvgAge")

# Replacing the average age
total.rows = nrow(titanic.test)
for (row.num in 1:total.rows) {
  if (is.na(titanic.test[row.num, "Age"])) {
    titanic.test[row.num, "Age"] = age.by.class.test[age.by.class.test$PClass == titanic.test[row.num, "Pclass"], ]$AvgAge
  }
}

# Removing the columns that aren't required 
library(dplyr)
titanic.test.new = select(titanic.test, -PassengerId, -Name, -Ticket, -Cabin)

# Changing the columns to factor 
titanic.test.new$Pclass = factor(titanic.test.new$Pclass)
titanic.test.new$Parch = factor(titanic.test.new$Parch)
titanic.test.new$SibSp = factor(titanic.test.new$SibSp)
titanic.test.new$Sex = factor(titanic.test.new$Sex)
titanic.test.new$Embarked = factor(titanic.test.new$Embarked)
# unique(titanic.test.new$Parch)
# unique(titanic.train.new$Parch)
# View(titanic.test.new)
########## Predicting the values ##########
fitted.probabilities = predict(object = log.model.titanic, newdata = titanic.test.new, type = 'response')
####
# When I ran the above line of code for the first time it gave the following error
##
# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels): 
# factor Parch has new levels 9
##
# What it means is that for Parch there was a new value in test set that wasn't in train
# set. There were 2 rows with Parch = 9. As there were no rows with Parch = 9 in train set
# the model wasn't aware of how to handle that hence an error was thrown. There are 3 
# possible solutions for this - 
# 1. Remove the rows for Parch = 9 from test set
# 2. Add rows for Parch = 9 in train set
# 3. Replace the outliers with most occuring value
# As I did know how a row for Parch = 9 look like, I opted for option # 3. It didn't seem
# right to move one row with Parch = 9 from test set to train set because then the model
# will depend on a single row with Parch = 9 in train set to make predictions. All the code
# from "Cleaning the test dataset" was run again after deleting rows with Parch = 9 from 
# test set. The replaced value is 0. 
# Parch       Total Number in test set
#   0	            324
#   1	             52
#   3	              3
#   2	             33
#   4	              2
#   6	              1
#   5	              1
#   9	              2
# Formula for finding out unique values in excel is 
# =IFERROR(INDEX($G$2:$G$419, MATCH(0,COUNTIF($M$1:M1, $G$2:$G$419), 0)),"") 
# After entering the formula hit Ctrl + Shift + Enter
# https://www.extendoffice.com/documents/excel/4032-excel-dynamic-list-of-unique-values.html#a1
####
# fitted.probabilities has the probability of surviving. We aren't interested in 
# probabilities. We are interested in whether the passenger survives or not - 0 or 1; binomial
# output. So any probability > 0.5 will be considered as survived (1) and lesser will be 
# considered as not survived (0)
fitted.results = ifelse(fitted.probabilities > 0.5, 1, 0)


#



#




#############################################################################################
#############################################################################################
#############################################################################################
# Something done earlier
#############################################################################################
#############################################################################################
#############################################################################################
########## Logistic Regression ##########
titanic.train = read.csv(file = "C:\\Users\\dsharma\\Desktop\\R\\Udemy Course\\R-for-Data-Science-and-Machine-Learning\\Machine Learning with R\\titanic_train.csv")
# Finding the null values using Amelia package
library(Amelia)
missmap(obj = titanic.train, main = "Missing Map", col = c("Yellow", "Black"), legend = FALSE)
# Col argument - Yellow is for NA value and Black is for present value
# The missing values are in "age" column that have to be imputed. 
ggplot(data = titanic.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
# Deciding what to keep and what not to keep. This is a difficult question. The Name variable 
# has values like Mr, Mrs. Rev. etc that may be useful. Finding out the various salutation
# is a manual process.
index.mr = grep("Mr.", titanic.train$Name)
titanic.train$Salutation[index.mr] = "Mr"
index.mrs = grep("Mrs.", titanic.train$Name)
titanic.train$Salutation[index.mrs] = "Mrs"
index.miss = grep("Miss.", titanic.train$Name)
titanic.train$Salutation[index.miss] = "Miss"
index.mstr = grep("Master.", titanic.train$Name)
titanic.train$Salutation[index.mstr] = "Master"
index.don = grep("Don.", titanic.train$Name)
titanic.train$Salutation[index.don] = "Don"
index.rev = grep("Rev.", titanic.train$Name)
titanic.train$Salutation[index.rev] = "Rev"
index.dr = grep("Dr.", titanic.train$Name)
titanic.train$Salutation[index.dr] = "Dr"
index.mme = grep("Mme.", titanic.train$Name)
titanic.train$Salutation[index.mme] = "Mrs"
index.ms = grep("Ms.", titanic.train$Name)
titanic.train$Salutation[index.ms] = "Miss"
index.major = grep("Major.", titanic.train$Name)
titanic.train$Salutation[index.major] = "Army"
index.mlle = grep("Mlle.", titanic.train$Name)
titanic.train$Salutation[index.mlle] = "Miss"
index.col = grep("Col.", titanic.train$Name)
titanic.train$Salutation[index.col] = "Army"
index.capt = grep("Capt.", titanic.train$Name)
titanic.train$Salutation[index.capt] = "Army"
index.countess = grep("Countess.", titanic.train$Name)
titanic.train$Salutation[index.countess] = "Royalty"
index.jonkheer = grep("Jonkheer.", titanic.train$Name)
titanic.train$Salutation[index.jonkheer] = "Royalty"
titanic.train$Salutation = factor(titanic.train$Salutation)
# summary(titanic.train$Salutation)
# index.na = which(is.na(titanic.train$Salutation))
# index.na
# titanic.train$Name[823]
# After getting the salutation information the name does not hold any value for us. Similarly
# Passenger id is also of no use. Saving those columns that make sense, adding new columns
# based on requirement, transforming existing columns to factors, characters, etc is called
# FEATURE ENGINEERING. 

remove.columns = c("PassengerId", "Name", "Ticket", "Cabin")
titanic.train = titanic.train[,!names(titanic.train) %in% remove.columns]

titanic.train$Survived = factor(titanic.train$Survived)
titanic.train$Pclass = factor(titanic.train$Pclass)
titanic.train$Sex = factor(titanic.train$Sex)
titanic.train$Embarked = factor(titanic.train$Embarked)
titanic.train$Salutation = factor(titanic.train$Salutation)
titanic.train$FamilySize = titanic.train$SibSp + titanic.train$Parch

# Imputing the age data using caret
library(caret)
# Imputation method in caret workss only on numeric data. They do not work for factors. 
# So converting the factors to numeric data. This is done by dummy variables. In python
# it is called as one hot encoding. dummyVars helps us to convert the factors to numeric.
# Pclass factor variable with 3 values 1, 2, & 3 is converted to 3 different variables
# Pclass.1, Pclass.2 & Pclass.3. Whereever there was a value of Pclass was 1, Pclass.1 = 1, 
# Pclass.2 = 0, Pclass.3 = 0 and so on. Except for survived variable everything is used.
dummy.vars = dummyVars(~., data = titanic.train[,-1])
train.dummy = predict(dummy.vars, titanic.train[,-1])
View(train.dummy)
# Impute
pre.process = preProcess(train.dummy, method = "bagImpute")
imputed.data = predict(pre.process, train.dummy)
View(imputed.data)

titanic.train$Age = imputed.data[,6]
View(titanic.train)
# Checking for NA values
missmap(obj = titanic.train, legend = FALSE, main = "Imputed Data", col = c("Yellow", "Black"))
# Creating Model
log.model.titanic = glm(formula = Survived~., family = binomial(link = "logit"), 
                        data = titanic.train)
summary(log.model.titanic)
