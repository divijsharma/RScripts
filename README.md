# RScripts
## Sentiment Analysis.R    
  1. Code for cleaning Twitter specific text. This includes cleaning "RT", "via", "@", handle names, links, #tags, time with AM/P
  2. Finding sentiment score for the text using syuzhet package

## WordCloud.R
  1. Create Corpus
  2. Create TDM
  3. Create WordCloud

## ML - Model - kNN.R
  1. Decide whether the data needs to be normalized or not based on variance
  2. Split the data into train and test set based on the distribution of target variable
  3. Run the kNN model multiple times to find the best k value and least error rate
  4. Create csv file with the predictions for the best value of k

## YouTube Analysis.R
  1. Connecting to YouTube from R using API Key from http://developers.google.com account
  2. Creating User Network to understand how users have interacted with each other using comments section
  3. Sentiment Analysis of comments based on score
