########## My code to find out the missing data ##########
missing.data = function(x){
  total.rows = nrow(x)
  total.cols = ncol(x)
  col.names = names(x)
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
                               Class = character(length(columns.with.null)),
                               stringsAsFactors = FALSE)
  
  for (i in 1:length(columns.with.null)){
    missing.data.df$Column[i] = columns.with.null[i]
    missing.data.df$Count[i] = nrow(NA.Values[NA.Values$Column == columns.with.null[i],])
    missing.data.df$Class[i] = class(x[, columns.with.null[i]])
  }
  return(missing.data.df)
}