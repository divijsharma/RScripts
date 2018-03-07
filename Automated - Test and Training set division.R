# Dividing the dataset in 10 different training and test sets. 
########## Option 1 - Using caTools package ##########
library(caTools)
# We want to predict G3 - grade of third class of the course. We can give any column
# name but it is a good practice to provide the column name of data set same as the
# one we are trying to predict.
# Setting seed before every split so that we ge the same data again and again.

for (i in 1:10){
  set.seed(i)
  assign(paste("split", i, sep = ""), sample.split(Y = student.data$G3, SplitRatio = 0.7))
  expr.train =  paste("split", i, "== TRUE", sep = "")
  expr.test = paste("split", i, "== FALSE", sep = "")
  assign(paste("train", i, sep = ""), subset(student.data, eval(parse(text = expr.train))))
  assign(paste("test", i, sep = ""), subset(student.data, eval(parse(text = expr.test))))
  expr.rm = paste("rm(split", i, ")", sep = "")
  eval(parse(text = expr.rm))
} 

########## Option 2 - No special package ##########
dataset.name = "student.data"
for(i in 1:10){
  set.seed(i+20)
  assign(paste("indices", i, sep = ""), sort(sample(nrow(eval(parse(text = dataset.name))), nrow(eval(parse(text = dataset.name)))*.7)))
  expr.train = paste(dataset.name,"[indices", i,",]",sep = "")
  assign(paste("train.opt2.", i, sep = ""), eval(parse(text = expr.train)))
  expr.test = paste(dataset.name,"[-indices", i,",]",sep = "")
  assign(paste("test.opt2.", i, sep = ""), eval(parse(text = expr.test)))
  expr.rm = paste("rm(indices", i, ")", sep = "")
  eval(parse(text = expr.rm))
}

########## Deleting the datasets created ##########
for (i in 1:10){
  expr.rm = paste("rm(test.opt2.", i, ")", sep = "")
  eval(parse(text = expr.rm))
  expr.rm = paste("rm(train.opt2.", i, ")", sep = "")
  eval(parse(text = expr.rm))
  expr.rm = paste("rm(test", i, ")", sep = "")
  eval(parse(text = expr.rm))
  expr.rm = paste("rm(train", i, ")", sep = "")
  eval(parse(text = expr.rm))
  rm(expr.train)
  rm(expr.test)
  rm(expr.rm)
  rm(expr)
}