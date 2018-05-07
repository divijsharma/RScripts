################### Linear regression - line for a list of scattered points ###################
# UsingR has a dataset called father.son that we want to use for the plotting. The dataset has list
# of father height and son height
library(UsingR)
library(graphics)
library(grDevices)
library(ggplot2)
# We start with a scatter plot for every point in the dataset. How does a linear model works?
# Linear model is nothing but a straight line that best fits the points. For this we calculate 
# the distance of each point to the assumed line that best fits the points in data frame. This 
# distance is called residual. We square the residual and take sum of all square residuals. 
# The line that best fits the points is the one with smallest sum of square residual distances
# This is done directly in R using lm() function
linear_model = lm(father.son$sheight~father.son$fheight)
linear_model
summary(linear_model)
ggplot(data = father.son, aes(x = father.son$fheight, y = father.son$sheight)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", formula = y ~ x,  se = FALSE)
# In the above line se is the confidence level. TRUE is by default. 
# Till here we have a scatter plot and regression line running through it. We now want to put the 
# equation of line and residual suare also in the plot. 
################### Function to get the line equation and R-Squared as string for a linear model ################### 
lm_equation = function(model) {
  intercept = coef(model)[1]
  slope = coef(model)[2]
  slope_abs = abs(slope)
  r2 = summary(model)$r.squared
  
  intercept_string = format(intercept, digits = 2)
  slope_abs_string = format(slope_abs, digits = 2)
  r2_string = format(r2, digits = 3)
  
  variable_list = list(intercept = intercept_string,
                       slope = slope_abs_string,
                       r2 = r2_string
  )
  substitute(italic(y) == slope %.% italic(x) + intercept*","~~italic(r)^2~"="~r2,variable_list)
  if (slope >= 0) {
    if (intercept >= 0) {
      equation = substitute(italic(y) == slope %.% italic(x) + intercept*","~~italic(r)^2~"="~r2,variable_list)
    } else {
      equation = substitute(italic(y) == slope %.% italic(x) - intercept*","~~italic(r)^2~"="~r2,variable_list)
    }
  } else {
    if (intercept >= 0) {
      equation = substitute(italic(y) == -slope %.% italic(x) + intercept*","~~italic(r)^2~"="~r2,variable_list)
    } else {
      equation = substitute(italic(y) == -slope %.% italic(x) - intercept*","~~italic(r)^2~"="~r2,variable_list)
    }
  }
  return(as.character(as.expression(equation)))  
}

################### Linear regression - line for a list of scattered points & Equation ###################
# The red line is called least squares line
ggplot(data = father.son, aes(x = father.son$fheight, y = father.son$sheight)) +
  geom_point()+
  geom_smooth(method = "lm", color = "red", formula = y ~ x,  se = FALSE) +
  geom_text(x = 65, y = max(father.son$sheight), label = lm_equation(linear_model), parse = TRUE)

linear_model
################### How to predict using Linear Regression Model ################### 
# Step 1 - Create the model
sheight = father.son$sheight
fheight = father.son$fheight
lm_father_son_height = lm(sheight~fheight)
# lm_father_son_height = lm(father.son$sheight~father.son$fheight)
# Step 2 - Store Slope and coeffiect of the liner model
intercept_linear_model = coef(lm_father_son_height)[1]
slope_linear_model = coef(lm_father_son_height)[2]
# Step 3 - Predict the new value of Y based on the data
# Method 1
new_father_height = 71.2
fheight = 71.2
new_son_height = slope_linear_model * new_father_height + intercept_linear_model
new_son_height
intercept_linear_model
slope_linear_model
# Method 2 
new_son_height2 = predict(lm_father_son_height, data.frame(fheight))
new_son_height2

# Why sheight = father.son$sheight & fheight = father.son$fheight are added
# The model is created with father.son$sheight~father.son$fheight and predicted using predict
# method we get a warning 
# Warning message:
#  'newdata' had 1 row but variables found have 1078 rows
# and the model returns 1078 rows. This is because the model is created with 
# father.son$fheight and prediction is done with fheight. This creates a conflict within the 
# predict method.
# Refer below link for more details. 
# http://stackoverflow.com/questions/27464893/getting-warning-newdata-had-1-row-but-variables-found-have-32-rows-on-pred

summary(linear_model)

########## Row Slicing Data Frame ##########
# Read the sample file
dataset = read.csv(file = file.choose(), header = TRUE)
summary(dataset)
library(ggplot2)
ggplot(data = dataset, aes(x = x, y = y)) +
  geom_point()

log_data = log(dataset)
ggplot(data = log(dataset), aes(x = x, y = y)) +
  geom_point()
ggplot(data = log_data, aes(x = x, y = y)) +
  geom_point()

# Sampling some of the rows
indices = sort(sample(nrow(dataset), nrow(dataset)*.6))
trainset = dataset[indices,]
testset = dataset[-indices,]

# Plotting Train & Test set
ggplot() +
  geom_point(data = trainset, aes(x = x, y = y), color = "red") +
  geom_point(data = testset, aes(x = x, y = y), color = "blue")

ggplot(data = trainset, aes(x = x, y = y)) +
  geom_point() +
  ggtitle("Train Set")

ggplot(data = testset, aes(x = x, y = y)) +
  geom_point() +
  ggtitle("Test Set")

# Checking if the data is normally distributed or not
ggplot(data = dataset) +
  geom_histogram(aes(x = x, y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$x), sd = sd(dataset$x)), aes(x = dataset$x))

shapiro.test(dataset$x)

ggplot(data = dataset) +
  geom_histogram(aes(x = y, y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(dataset$y), sd = sd(dataset$y)), aes(x = dataset$y))

shapiro.test(dataset$y)

# From the graphs we interpret that x is normally distributed and y is not. Same 
# interpretation is from shapiro test
# Reading the training and test set provided in the lecture.
# As the files provided are not csv but xlsx files using xlsx package to read the data
library(xlsx)
train_lecture = read.xlsx(file = "C:\\Users\\dsharma\\Desktop\\R\\Test Files\\Train.xlsx", sheetName = "train", header = TRUE)
test_lecture = read.xlsx(file = "C:\\Users\\dsharma\\Desktop\\R\\Test Files\\Test.xlsx", sheetName = "test", header = TRUE)

linear_model = lm(y~x, data = train_lecture)
x = c(1,9)
y= predict(linear_model, data.frame(x))
y
# Plotting the least square line - The linear model line
ggplot(data = train_lecture, aes(x = x, y = y)) +
  geom_point()+
  geom_smooth(method = "lm", color = "red", formula = y ~ x,  se = FALSE) +
  ggtitle("Train Set with Least Squares Line")
# Plotting the leaset squares line for training set to test set
ggplot() +
  geom_point(data = test_lecture, aes(x = x, y = y)) +
  geom_smooth(data = train_lecture, aes(x = x, y = y), method = "lm", color = "red", formula = y ~ x, se = FALSE) +
  ggtitle("Test Set with Least Squares Line of Train Set")

# Calculating Residual sum of squares for test set based on least squares line of training set
# In order to calculate this we will subtract the y values of test set with the predicted 
# values of y from the model for the values of x in test set and then sum the square of 
# differences.
# Predicting values of x
predict(linear_model, test_lecture)
# Since test_lecture is already a data frame and the column name x is same as that used to 
# create model we can pass test_lecture directly.
# Calculating residual sum of squares
residual_square = sum((test_lecture$y - predict(linear_model, test_lecture))^2)
residual_square
# Calculating mean squared error (MSE). Instead of summing the square of differences, we take
# mean of them. 
mse_test = mean((test_lecture$y - predict(linear_model, test_lecture))^2)
mse_test

# Generating Quadratic Model
quadratic_model = lm(y~x+I(x^2), data = train_lecture)
quadratic_model

# Plotting linear and quadratic model at the same time
ggplot(data = train_lecture, aes(x = x, y = y)) +
  geom_point(color = "lightgreen")+
  geom_smooth(method = "lm", color = "red", formula = y ~ x,  se = FALSE) +
  geom_smooth(method = "lm", formula = y~poly(x,2), color = "blue", se = FALSE) +
  #  geom_point(data = data.frame(x = train_lecture$x, y = predict(linear_model, train_lecture)), color = "gold") +
  #  geom_point(data = data.frame(x = train_lecture$x, y = predict(quadratic_model, train_lecture)), color = "yellow") +
  ggtitle("Training Set with 2 models")

ggplot(data = train_lecture, aes(x = x, y = y)) +
  geom_point(color = "lightgreen")+
  geom_smooth(method = "lm", color = "red", formula = y ~ x,  se = FALSE) +
  geom_smooth(method = "lm", formula = y~poly(x,2), color = "blue", se = FALSE) +
  #  geom_point(data = data.frame(x = train_lecture$x, y = predict(linear_model, train_lecture)), color = "gold") +
  #  geom_point(data = data.frame(x = train_lecture$x, y = predict(quadratic_model, train_lecture)), color = "yellow") +
  ggtitle("Test Set with 2 models")

# Calculating MSE for Quadratic Model
mse_quad_test = mean((test_lecture$y - predict(quadratic_model, test_lecture))^2)
mse_quad_test

ggplot(data = train_lecture, aes(x = x, y = y)) +
  geom_point(color = "lightgreen")+
  geom_smooth(method = "lm", color = "red", formula = y ~ x,  se = FALSE) +
  geom_smooth(method = "lm", formula = y~poly(x,2, raw = TRUE), color = "blue", se = FALSE) +
  geom_smooth(method = "lm", formula = y~poly(x,10,raw = TRUE), color = "yellow", se = FALSE) +
  #  geom_point(data = data.frame(x = train_lecture$x, y = predict(linear_model, train_lecture)), color = "gold") +
  #  geom_point(data = data.frame(x = train_lecture$x, y = predict(quadratic_model, train_lecture)), color = "yellow") +
  ggtitle("Test Set with multiple models")

########## For Loop for calculating Test MSE for multiple models ##########
cubic_model = lm(y~poly(x,3,raw = TRUE), data = train_lecture)
cubic_mse = mean((train_lecture$y - predict(cubic_model, test_lecture))^2)

# Function to calculate MSE
mse_function = function(i, train, test){
  # create dummy varialble to store numeric
  mse = numeric()
  for (i in 1:n) {
    model = lm(y~poly(x, i, raw = TRUE), data = train)
    mse[i] = mean((test$y - predict(model, test))^2)  
  }
  return(mse)
}

# The "variable" part of code starts here.
indices = sort(sample(nrow(dataset), nrow(dataset)*.6))
trainset = dataset[indices,]
testset = dataset[-indices,]

n = 10
mse_test_loop = mse_function(i = n, train = trainset, test = testset)

# Visualizing MSEs of various models
# mse_test_loop has only MSE values. In order to create a graph we need a data frame with x
# & y. 
mse_df = data.frame(x = c(1:n), y = mse_test_loop)
# Now use this data frame in ggplot
ggplot(data = mse_df, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_point(size = 3) +
  xlab("Polynomial Degree") +
  ylab("Test MSE")
# The above will create a different plot for every sample. Let's say we want to see the plot
# for m different samples

mse_function = function(n, train, test){
  # create dummy varialble to store numeric
  mse = numeric()
  for (i in 1:n) {
    model = lm(y~poly(x, i, raw = TRUE), data = train)
    mse[i] = mean((test$y - predict(model, test))^2)  
  }
  return(mse)
}

# The "variable" part of code starts here.

n = 10
plot = ggplot()
for (m in 1:10) {
  indices = sort(sample(nrow(dataset), nrow(dataset)*.6))
  trainset = dataset[indices,]
  testset = dataset[-indices,]
  mse_test_loop = mse_function(n = n, train = trainset, test = testset)
  mse_df = data.frame(x = c(1:n), y = mse_test_loop)
  plot = plot + geom_point(data = mse_df, aes(x = x, y = y), size = 2)
  plot = plot + geom_line(data = mse_df, aes(x = x, y = y), color = "blue")
  plot = plot + xlab("Polynomial Degree")
  plot = plot + ylab("Test MSE")
  
}
plot
