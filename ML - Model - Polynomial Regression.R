########## Read data in R ########## 
poly_data = read.csv(file = "C:\\Users\\dsharma\\Desktop\\R\\Test Files\\Dat.csv")

########## Plotting the data using ggplot
ggplot(data = poly_data, aes(x = x, y = y)) +
  geom_point(color = "darkgreen", size = 1)
# Creating Linear Model for this data
linear_model_lecture = lm(y~x, data = poly_data)
# Plotting the linear model line also with scatter plot
ggplot(data = poly_data, aes(x = x, y = y)) +
  geom_point(color = "darkgreen", size = 1) +
  geom_smooth(method = "lm", formula = y~x, color = "red", se = FALSE)
y_linear_model = function(x) {
  y = coef(linear_model_lecture)[2] * x + coef(linear_model_lecture)[1]
}

calculated_y = y_linear_model(poly_data$x)
points_on_least_square_line = data.frame(x = poly_data$x, y = calculated_y)
# These points are the corresponding points on least squared line for every x of the input data
# Plotting these points also along with the previous plot
plot = ggplot(data = poly_data, aes(x = x, y = y)) +
  geom_point(color = "darkgreen", size = 1) +
  geom_smooth(method = "lm", formula = y~x, color = "red", se = FALSE) +
  geom_point(data = points_on_least_square_line, aes(x = x, y = y), color = "black")
plot
########## Creating residuals ########## 
# Residuals are distance of perpendicular lines from each point of the scatter plot to the least
# square line. The name least square line comes from the fact that sum of squares of residual is
# least for this line. The way we create the residual lines is to group each (x,y) coordinate at
# least squares line with the corresponding (x,y) coordinate of the actual data and then create a
# line between these two points.
# Each row of input data and the data frame storing (x,y) coordinates of least squared line - 
# poly_data and points_on_least_square_line should be numbered. For that first find out the number 
# of rows in the data frame. Sonce we have created points_on_least_square_line from poly_data they
# will have same number of rows
number_rows = NROW(poly_data)
# Add group columns to each data frame
poly_data$group = 1:number_rows
points_on_least_square_line$group = 1:number_rows
# head(poly_data)
# head(points_on_least_square_line)
# Till this point the rows in each data frame with same value in x column will have same values in 
# group column. FOr rbind to work, the column names of  data frames should be same. 
names(points_on_least_square_line) = c("x", "y", "group")
binded_data = rbind(poly_data, points_on_least_square_line)
plot = plot + geom_line(data = binded_data, aes(x = x, y = y, group = group))
# The above creates a line for a specific group. Every (x,y) coordinate of same group is joined
plot
########## Calculation of Residual Squared ########## 
residual_squared = sum((poly_data$y - points_on_least_square_line$y)^2)
residual_squared
# another way is to get it directly from the linear model we created 
residual_squared_model = summary(linear_model_lecture)$r.squared
residual_squared_model

########## Polynomial equation ########## 
# Creating model for polynomial regression
quadratic_model = lm(y~x+I(x^2), data = poly_data)
quadratic_model
y_quadratic_model = function(x){
  y = coef(quadratic_model)[3]*x^2 + coef(quadratic_model)[2] * x + coef(quadratic_model)[1]
}

calculated_y_quadratic = y_quadratic_model(poly_data$x)
points_on_quadratic_curve = data.frame(x = poly_data$x, y = calculated_y_quadratic)

plot_quadratic = ggplot(data = poly_data, aes(x = x, y = y)) +
  geom_point(color = "darkgreen", size = 1) +
  geom_smooth(method = "lm", formula = y~poly(x,2), color = "red", se = FALSE) +
  #  geom_smooth(method = "lm", formula = y~x+I(x^2), color = "red", se = FALSE) +
  # The above 2 are same thing. It is easier to use poly function. For a cubic
  # equation we can either use poly(x,3) or x+I(x^2)+I(x^3)  
  geom_point(data = points_on_quadratic_curve, aes(x = x, y = y), color = "black")
plot_quadratic

########## Smoothing Splines ##########
library(splines)
smooth_spline_model = smooth.spline(x = poly_data$x, y = poly_data$y, df = 50)
# df = degrees of freedom. This controls how rough the curve will be. If df is more then the
# curve will be more wiggly and better fit and if df is less then the curve is smoother. 
smooth_spline_model
# Predicting one value in smoothing spline. predict(smooth_spline_model, 10) will output in 
# both input x value and predicted y value. In order to get only y value we use the following 
predicted_y = predict(smooth_spline_model, 10)$y

# Funciton to get predicted y values for every x value in a data frame. The output of the 
# prediction of smooth.spline model is always x and y. So irrespective of the column names
# in data frame the output is always x and y
smooth_spline_function = function(x){
  return(predict(smooth_spline_model, x)$y)
}

# Plotting smoothing splines in ggplot
smooth_spline_plot = ggplot() +
  geom_point(data = poly_data, aes(x = x, y = y)) +
  stat_function(data = data.frame(x = c(-2, 14)), aes(x = x), fun = smooth_spline_function)

# Getting points on the curve
# Creating a data frame for only x values of poly_data whcih will be used for prediction and 
# curve plotting
poly_data_curve = data.frame(poly_data$x)
names(poly_data_curve) = c("x")
poly_data_curve$y = smooth_spline_function(poly_data_curve$x)

smooth_spline_plot = smooth_spline_plot + 
  geom_point(data = poly_data_curve, aes(x = x, y = y), color = "red")
smooth_spline_plot

# Execute the Smoothing Splines code with df= 50 and df = 10
# With df = 50 the smooth slipne curve will follow the data too much and is definatly as 
# better for the training set data. The model may not be good for the data set used for
# prediction. This is called overfititng. 

########## Creating ploynomial regression of nth degree ##########
lm_degree_10 = lm(y~poly(x,10,raw = TRUE), data = poly_data)
lm_degree_10
