########## kNN for caravan data - MY CODE ##########
library(ISLR)
########## Read Data ##########
input.caravan = Caravan
# It is very important to normalilze the data for kNN. In order to understand whether normalization is 
# required or not, find the variance of each column. If the variance varies significantly then we should
# normalize the data.
########## Finding variance of each column - Decision on Normalization ##########
total.cols = ncol(input.caravan)
variance = numeric()
# The last column is a factor column so no need to find variance of that
for (i in 1:total.cols - 1){
  variance[i] = var(input.caravan[,i])
}
# Find the difference in variance of each column
diff.in.var = max(variance) - min(variance)

# If variance is more than 1 then standardize the columns. The last column is the target variable.
# Normalization should be done only for numeric columns
if (diff.in.var > 1){
  input.caravan[,-total.cols] = scale(input.caravan[,-total.cols])
}
########## Split the data into Train and Test ##########
# First check the distribution of target variable. If the data is skewed then the split should take care
# of that skewness
caravan.purchase.distribution = round(prop.table(table(input.caravan$Purchase)) * 100, digits = 1)
# caravan.purchase.distribution
total.cols = length(caravan.purchase.distribution)
equal.distribution = 100 / total.cols
max.percent = max(caravan.purchase.distribution)
min.percent = min(caravan.purchase.distribution)
# Checking if the max and min percent of rows are within +- 10 of equal distribution or not.
if ((max.percent <= equal.distribution + 10 | max.percent >= equal.distribution) & (min.percent <= equal.distribution + 10 | min.percent >= equal.distribution)){
  # If the distribution is in the permissible limit then randomly split the data
  sample.size = floor(0.75 * nrow(input.caravan))
  set.seed(123)
  train.ind = sample(seq_len(nrow(input.caravan)),size = sample.size)
  caravan.train = input.caravan[train.ind, ]
  caravan.test = input.caravan[-train.ind, ]
}else{
  # The distribution is not in permissible limit split the data based on distribution of target variable
  unique.target = unique(input.caravan$Purchase)
  splits.to.be.made = length(unique.target)
  # Dynamically split the input data into multiple files
  for (i  in 1:splits.to.be.made) {
    temp.split = input.caravan[, input.caravan$Purchase == unique.target[i]]
    sample.size = floor(0.75 * nrow(temp.split))
    set.seed(123)
    train.ind = sample(seq_len(nrow(temp.split)),size = sample.size)
    temp.train = temp.split[train.ind, ]
    temp.test = temp.split[-train.ind, ]
    caravan.train = merge(caravan.train, temp.train)
    caravan.test = merge(caravan.test, temp.test)
  }
}
caravan.test.purchase.distribution = round(prop.table(table(caravan.test$Purchase)) * 100, digits = 1)
caravan.train.purchase.distribution = round(prop.table(table(caravan.train$Purchase)) * 100, digits = 1)
# caravan.train.purchase.distribution
# caravan.test.purchase.distribution
caravan.test.purchase.dist.max.diff = abs(max(caravan.test.purchase.distribution) - max(caravan.train.purchase.distribution))
caravan.test.purchase.dist.min.diff = abs(min(caravan.test.purchase.distribution) - min(caravan.train.purchase.distribution))
# Splitting the data and class for test and train
caravan.test.purchase = caravan.test[,86]
caravan.test.data = caravan.test[,-86]
caravan.train.purchase = caravan.train[,86]
caravan.train.data = caravan.train[,-86]
if (caravan.test.purchase.dist.max.diff <= 5 & caravan.test.purchase.dist.min.diff <= 5){
  # If the test and training set distribution are in the premissible limit continue with creating kNN
  # model and prediction
  ########## Model Creation ##########
  # Load class library that has kNN
  library(class)
  number.of.iterations = 20
  predicted.caravan.purchase = NULL
  error.rate = data.frame(KValue = numeric(number.of.iterations), 
                          ErrorRate = numeric(number.of.iterations))
  for (i in 1:number.of.iterations) {
    predicted.caravan.purchase = knn(train = caravan.train.data, test = caravan.test.data, cl = caravan.train.purchase, k = i)
    error.rate$ErrorRate[i] = mean(caravan.test.purchase != predicted.caravan.purchase)
    error.rate$KValue[i] = i
  }
  # Visualization of K and error rate
  library(ggplot2)
  error.rate.plot = ggplot(data = error.rate, aes(x = KValue, y = ErrorRate)) +
    geom_point() +
    geom_line() +
    theme_bw()
  print(error.rate.plot)
  ggsave(filename = "C:\\Users\\dsharma\\Desktop\\R\\Test Files\\Prediction\\knn_error_rate_plot.png", plot = error.rate.plot)
  # Finding K associated with minimum error rate. Using outermost min() to find the k at which elbow 
  # starts
  best.k = min(error.rate$KValue[error.rate$ErrorRate == min(error.rate$ErrorRate)])
  min.error.rate = min(error.rate$ErrorRate[error.rate$ErrorRate == min(error.rate$ErrorRate)])
  print(paste("Best K value is = ", best.k))
  print(paste("Error Rate is", round(x = min.error.rate, digits = 4)))
  # Run kNN once more for best.k so that we can get the prediction
  best.prediction = knn(train = caravan.train.data, test = caravan.test.data, cl = caravan.train.purchase, k = best.k)
  # Convert from factor to dataframe to rename the column name
  best.prediction.df = as.data.frame(x = best.prediction)
  colnames(best.prediction.df) = "Purchase"
  # Create the final dataframe with test data and prediction
  final.df.with.prediction = cbind(best.prediction.df, caravan.test.data)
  # Saving the file in disk
  write.csv(x = final.df.with.prediction, file = "C:\\Users\\dsharma\\Desktop\\R\\Test Files\\Prediction\\kNN_Caravan.csv", row.names = FALSE)
  print("Final Predictionas are saved in C:\\Users\\dsharma\\Desktop\\R\\Test Files\\Prediction\\kNN_Caravan.csv")
  #
}else{
  # WRITE CODE TO TERMINATE THE PROGRAM
  # Still to find. Could not find anything of importance. quit(), stop() etc are not effective.
  print("Could not create the test and train files with the same class distribution as input data")
}

library(gmodels)
CrossTable(x = caravan.test.purchase, y = best.prediction, prop.chisq=FALSE)
########## kNN for Caravan Data - MY CODE ENDS HERE ##########






########## kNN using iris dataset ##########
# We are going to look at a simple dataset for 3 different species of Iris plant

# The blelow command loads the data into memory. When the data is loaded it is there in "Values"
# area of environment as <Promise>
data(iris)
# To move the loadedd iris data into the "Data" area of the environment so that we can see it
iris

# Step 1 for machine learning is to load the data into R and assign column names using names() command.
# In order to use the names() we must know what each column of the data represents. Hence an initial 
# understanding of the data you are using is a must. 

# Step 2 - Profound understanding of the data. This includes understanding the structure of data, data
# type of each column

# Looking at the first few rows of the dataset can give some clarity on how the data is stored and
# various values possible for a column
head(iris)

# Understanding the structure of data is very important before we begin to create any model.
str(iris)
# Using the str() command we come to know that all the columns except for Species are numeric. Species
# is a factor with 3 levels implies that Species is a ctegorical variable where as all other variables 
# are continuous variables. Many R machine learning classifiers require that the target feature is coded
# as factor. That's why it is important to find out the structure of dataset - to help us determine
# the target feature - one for which the prediction is based. In this excercise we are going to predict
# Species of Iris plant based on input value of other numeric field.

# Understanding the distribution of data 
summary(iris)
summary(iris[c("Petal.Width", "Sepal.Width")]) # for specific columns
# summary() command gives the details of minimum, maximum, median, mean, 1st quarter, 3rd quarter for
# numeric fields and number of observations for each level of factor variables. It is important to have 
# equally distributed data for each level of factor data. If there are say 500 rows for species one, 
# 50 rows for each of species 2 & 3 then there is a possibiltiy of skewed result for kNN algorithm. It
# important that the data is distributed equally with respect to target feature.

table(iris$Species)
round(prop.table(table(iris$Species)) * 100, digits = 1)
# For prop.table see - http://www.statmethods.net/stats/frequencies.html

# Step 3 - Create plots on the data set to understand the corellation of various columns with each 
# other. Scatter plots help in understanding how various variables are corelated with each other. 
# There are 2 ways to create scatter plots ggvis and ggplot2. ggplot2 is better as we can overlay 
# multiple layers on top of each other.
library(ggvis)
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
# This gives the relation between Sepal Length and Width
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()
# This gives the relation between Petal Length and Width

# !!!!!!!!!!!!!!!!!! It is very important to have a basic understanding of the dataset so that we can 
# take a decision on finding corelation between which columns. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Step 4 - Normalize the data if required. In the summary() you will see the min and max of a variable.
# All the variables in the dataset must lie within a specific limit of each other. If one of the 
# variables X has value from 1 to 100 and other variable Y has a range of 1 to 1000, then variable Y's
# influence on distance calculation will overpower X's influence. To overcome this we need to normalize
# the dataset so that the distance between variables with larger range will not be overemphasised.
# Normalization must be done only for numeric columns. Normalizing factor column will result in error.
# 'min' not meaningful for factors
normalize <- function(x) {
  return(((x - min(x))/(max(x) - min(x))))
}
normalizedIris <- as.data.frame(lapply(iris[1:4], normalize))
# normalizedIris will have only those columns that are mentioned in the lapply() function (numeric columns). 
# Add the species column
normalizedIris$Species <- iris$Species

# Step 6 - Creating Training and Test sets. The input dataset should be divided into two parts. One for
# training and another for testing the model. Usually 2/3rd of the dataset is used for training and 1/3rd of
# dataset is used for testing the model. The idea behind dividing the input dataset into 2 parts is once the 
# model is created - we will input the values from test dataset into the model and see if we get the same 
# result as the already available result of the test dataset. The percent of mismatch will tell us about the 
# accuracy of our model. Again the knowledge of dataset is important to divide the input dataset to training 
# test dataset. All the possible values of target variable must be represented equally in the training dataset.

# Dividing the dataset. We want to divide the iris dataset into 2 parts - one with 2/3rd of rows and another
# with 1/3rd of rows. We create a new variable called indicator and randomly assign 1 or 2 for each row of iris
# dataset. We will really update the iris dataset and add another column. The random numbers are generated 
# by sample() function. Before we generate the random sequence of 1s and 2s through sample() we set a seed.
# This is a number of R's random number generator. The major advantage of setting a seed is that you can get 
# the same sequence of random numbers whenever you supply the same seed in the random number generator.
set.seed(2711)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
# sample() generates a sequence of numbers from 1:x - x being the first argument. It will generate a sequence 
# of n number - n being the second argument. replace = TRUE implies that 1:x numbers may repeat in the 
# sequence. prob argument defines the probability of occurance of each number. The first number will occur
# 67% times and other will occur 33% times. 2 possible errors - 
# 1. x < n and replace = FALSE implies that we want to generate a sequence of, say, 1 & 2, say 150 times and
# do not want to replace the numbers. So the 1:x will each be used once and still there will be more spots to 
# fill. This results in error - cannot take a sample larger than the population when 'replace = FALSE'
# 2. The number of elements in probability vector is less than x. So we want to generate a sequence of say of
# 1s, 2s and 3s and have specified probability of occurance for only 2 numbers. This results in error - 
# incorrect number of probabilities

# Now that we have generated a sequence of 1s and 2s 150 times with probability of 67 & 33 % we divide the 
# dataset into 2 parts. Since iris dataset is already normalized we use the same dataset otherwise use the 
# normalized dataset created above.

iris.training <- iris[ind == 1, 1:4]
iris.test <- iris[ind == 2, 1:4]
iris.trainLabels <- iris[ind == 1, 5]
iris.testLabels <- iris[ind == 2, 5]
# In the training and test datsets we do not include the target variable. But the target variable is an input 
# to kNN algorithm so we create a vector separately.

# Step 7 - Build classifier
# knn() function in R uses Euclidean distance measure to find out k nearest neignbours. The value of K is
# provided by programmer.  In case of classification, the data point with the highest score wins the battle
# and the unknown instance receives the label of that winning data point. If there is an equal amount of
# winners, the classification happens randomly. k parameter is often odd to avoid ties in the voting scores.  
# Load class library that has knn function
library(class)
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=7)
# iris_pred will have the lables of target variable for every row of the test dataset. train is training 
# dataset. test is test dataset. cl is the true classification of target variable in train dataset. This is why
# we created a separate vector iris.trainLables. k is the number of nearest neighbours to be considered.

# Step 8 - Evaluating Model
# At this point we have 2 vectors for the lables of target variable related to test dataset. 
# 1. iris.testLables which has the actual data of target variable of test dataset
# 2. iris_pred which has the prediction of target variable of test dataset. 
# In order to find out the accuracy of our model we need to compare above two vectors. 
# Load gmodels library which has CrossTable function.
library(gmodels)
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
# the last argument prop.chisq indicates whether or not the chi-square contribution of each cell is included. 
# The chi-square statistic is the sum of the contributions from each of the individual cells and is used to 
# decide whether the difference between the observed and the expected values is significant.
# Looking at the output we can determine the success ratio of our model. values of k can be changed to improve
# the model and come to a correct model. 
# Below is the output for k = 3

#                 | iris_pred 
# iris.testLabels |     setosa | versicolor |  virginica |  Row Total | 
# ----------------|------------|------------|------------|------------|
#          setosa |         23 |          0 |          0 |         23 | 
#    |      1.000 |      0.000 |      0.000 |      0.319 |            |
#    |      1.000 |      0.000 |      0.000 |            |            | 
#    |      0.319 |      0.000 |      0.000 |            |            | 
# ----------------|------------|------------|------------|------------|
#      versicolor |          0 |         21 |          2 |         23 | 
#    |      0.000 |      0.913 |      0.087 |      0.319 |            | 
#    |      0.000 |      0.955 |      0.074 |            |            | 
#    |      0.000 |      0.292 |      0.028 |            |            | 
# ----------------|------------|------------|------------|------------|
#       virginica |          0 |          1 |         25 |         26 | 
#    |      0.000 |      0.038 |      0.962 |      0.361 |            | 
#    |      0.000 |      0.045 |      0.926 |            |            | 
#    |      0.000 |      0.014 |      0.347 |            |            | 
# ----------------|------------|------------|------------|------------|
#    Column Total |         23 |         22 |         27 |         72 | 
#    |      0.319 |      0.306 |      0.375 |            |            |
# ----------------|------------|------------|------------|------------|

# Below is the output for k = 5
#                 | iris_pred 
# iris.testLabels |     setosa | versicolor |  virginica |  Row Total | 
# ----------------|------------|------------|------------|------------|
#          setosa |         23 |          0 |          0 |         23 | 
#    |      1.000 |      0.000 |      0.000 |      0.319 |            |
#    |      1.000 |      0.000 |      0.000 |            |            | 
#    |      0.319 |      0.000 |      0.000 |            |            |
# ----------------|------------|------------|------------|------------|
#      versicolor |          0 |         22 |          1 |         23 | 
#    |      0.000 |      0.957 |      0.043 |      0.319 |            | 
#    |      0.000 |      0.957 |      0.038 |            |            | 
#    |      0.000 |      0.306 |      0.014 |            |            | 
# ----------------|------------|------------|------------|------------|
#       virginica |          0 |          1 |         25 |         26 | 
#    |      0.000 |      0.038 |      0.962 |      0.361 |            | 
#    |      0.000 |      0.043 |      0.962 |            |            | 
#    |      0.000 |      0.014 |      0.347 |            |            | 
# ----------------|------------|------------|------------|------------|
#    Column Total |         23 |         23 |         26 |         72 | 
#    |      0.319 |      0.319 |      0.361 |            |            | 
# ----------------|------------|------------|------------|------------|

# With k = 5 the error is reduced.


scaled.data.iris = scale(iris[1:4])
library(class)
library(caret)
indexes = createDataPartition(y = iris$Species, times = 1, p = 0.7, list = FALSE)
final.iris.data = cbind(scaled.data.iris, iris[5])
train.iris = final.iris.data[indexes, ]
test.iris = final.iris.data[-indexes, ]
for (i in 1:10){
  predicted.iris.knn = knn(train = train.iris[1:4],
                           test = test.iris[1:4],
                           cl = train.iris$Species,
                           k = i)
}
