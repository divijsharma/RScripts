#!!!!!!!! The latest code for missing values will be in the function script !!!!!!!!
########## My code defined as function in a different R script ##########
# Finding which columns have null values
source("C:\\Users\\dsharma\\Desktop\\R\\RStudio_Workspaces\\Automated - Finding NA function.R")
missing.data.df = missing.data(adult.sal.input)
missing.data.df

########## My code to find out the missing data ##########
x = data.frame(a = c(1, NA, 2), b = c(2, NA, 3), c = c(NA, "A", NA))
total.rows = nrow(x)
#total.rows
total.cols = ncol(x)
#total.cols
col.names = names(x)
#col.names
#x
# It is A MUST TO allocate the number of rows of NA.Values. An error comes otherwise.
NA.Values = data.frame(Column = character(total.rows * total.cols), 
                       Row = numeric(total.rows * total.cols), 
                       stringsAsFactors = FALSE)
NA.Values.Row.Count = 1
# NA.Values will have column name and row number of NA value. One column name may 
# apprear multiple times if more than one row have Na in the same column. Same row 
# may apprear more than once if 2 columns of the row have NA values. 
for (col.num in 1:total.cols){
  for (row.num in 1:total.rows){
    if (is.na(x[row.num,col.num])){
      # print(paste("Row = ", row.num))
      # print(paste("col = ", col.num))
      NA.Values$Column[NA.Values.Row.Count] = col.names[col.num]
      NA.Values$Row[NA.Values.Row.Count] = row.num
      NA.Values.Row.Count = NA.Values.Row.Count + 1
    }
  }
}

# Removing all the rows that do not have data
NA.Values = NA.Values[NA.Values$Row >0, ]
# Finding unique column values of NA.Values. These are the unique columns in the data
# that have missing values
columns.with.null = unique(NA.Values$Column)
# Creating the dataframe to store the columns with NA and the corresponding total 
# number of rows that have NA
missing.data.df = data.frame(Column = character(length(columns.with.null)), 
                             Count = numeric(length(columns.with.null)), 
                             Class =  character(length(columns.with.null)),
                             stringsAsFactors = FALSE)

for (i in 1:length(columns.with.null)){
  missing.data.df$Column[i] = columns.with.null[i]
  missing.data.df$Count[i] = nrow(NA.Values[NA.Values$Column == columns.with.null[i],])
  missing.data.df$Class = class(x[, columns.with.null[i]])
}
missing.data.df

########## Using Amelia Package ##########
# Titanic dataset will be used for this example
library(Amelia)
titanic.train = read.csv(file = 'C:\\Users\\dsharma\\Desktop\\Study\\Titanic\\train.csv',
                         header = TRUE,
                         stringsAsFactors = FALSE)

missmap(obj = titanic.train, 
        col = c('yellow', 'black'), 
        main = "Missing value map for Titanic")
# This will simply give out a plot with missing values. The number of rows in the plot are not
# very clear and plot is good enough just to find out if there are any missing values or not.

########## Using my code ##########
x = titanic.train
total.rows = nrow(x)
total.rows
total.cols = ncol(x)
total.cols
col.names = names(x)
# It is A MUST TO allocate the number of rows of NA.Values. An error comes otherwise. The 
# default number of rows in NA.Values dataframe is = total.rows * total.cols to accomodate
# the extreme scenario when all the columns of all the rows are NA. 
NA.Values = data.frame(Column = character(total.rows * total.cols), 
                       Row = numeric(total.rows * total.cols), 
                       stringsAsFactors = FALSE)
NA.Values.Row.Count = 1
for (col.num in 1:total.cols){
  for (row.num in 1:total.rows){
    if (is.na(x[row.num, col.num])){
      NA.Values$Column[NA.Values.Row.Count] = col.names[col.num]
      NA.Values$Row[NA.Values.Row.Count] = row.num
      NA.Values.Row.Count = NA.Values.Row.Count + 1
    }
  }
}

# Removing all the rows that have row value = 0. These rows of NA.Values have been created
# in the begining. This will ensure that we will have rows with data only. 
NA.Values = NA.Values[NA.Values$Row >0, ]
print(paste("Total missing values :", nrow(NA.Values)))
unique.columns = unique(x = NA.Values$Column)
print(paste("Total columns with missing values :", length(unique.columns)))
print(paste("Columns with missing values: ", unique.columns))
NA.Values

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
