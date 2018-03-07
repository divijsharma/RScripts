########## My code for splitting the data into training and test set based on the distribution ##########
#          of target variable
# This code does not take care of rownames. The rownames are lost during merge.
split.based.on.target = function(input.df, target.col.num, split.ratio, tolerance){
  
  df.target.distribution = round(prop.table(table(input.df[target.col.num])) * 100, digits = 1)
  total.cols = length(df.target.distribution)
  equal.distribution = 100 / total.cols
  max.percent = max(df.target.distribution)
  min.percent = min(df.target.distribution)
  # Removing the train and test sets if they are already created
  options(warn = -1) # Turning off the warnning if the data frame doesn't exist.
  rm(df.test, train.ind, df.train, unique.target, splits.to.be.made, temp.split, sample.size, temp.train, temp.test)
  options(warn = 0) # Turning back the warning
  # Creating the test and train dataset as per the input dataframe
  df.test = input.df[0,]
  df.train = input.df[0,]
  
  # Checking if the max and min percent of rows are within tolerance limit of equal distribution or not.
  if ((max.percent <= equal.distribution + tolerance & max.percent >= equal.distribution - tolerance) & (min.percent <= equal.distribution + tolerance & min.percent >= equal.distribution - tolerance)){
    # If the distribution is in the permissible limit then randomly split the data
    sample.size = floor(split.ratio / 100 * nrow(input.df))
    set.seed(123)
    train.ind = sample(seq_len(nrow(input.df)),size = sample.size)
    df.train = input.df[train.ind, ]
    df.test = input.df[-train.ind, ]
  }else{
    # The distribution is not in permissible limit split the data based on distribution of target variable
    unique.target = unique(input.df[,target.col.num])
    splits.to.be.made = length(unique.target)
    # Dynamically split the input data into multiple files
    for (i  in 1:splits.to.be.made) {
      temp.split = input.df[input.df[,target.col.num] == unique.target[i],]
      sample.size = floor(split.ratio / 100 * nrow(temp.split))
      # set.seed(123)
      train.ind = sample(seq_len(nrow(temp.split)),size = sample.size)
      temp.train = temp.split[train.ind, ]
      temp.test = temp.split[-train.ind, ]
      df.train = merge(x = temp.train, y = df.train, all = TRUE)
      df.test = merge(x = temp.test, y = df.test, all = TRUE)
    }
  }
  # Create a list of the train and test datadrame
  df.combined = list(df.train, df.test)
  return(df.combined)
}