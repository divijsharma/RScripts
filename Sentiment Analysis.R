########## Introduction to Sentiment Analysis ##########
# Sentiment Analysis finding the sentiment or emotions related to a topic based on 
# comments posted on it in various social media sources. Sentiment Analysis works on
# text. 
###
# Various tools available for Sentiment Analysis
# 1. Radian6 in Salesforce
# 2. EIKON from Thompson Reuters
# 3. SocialMention
# 4. Swipp
# 5. Sentiment Analysis in SAS
# 6. Trackur
# 7. Lexalytics
# 8. TheySay
# 9. NetOwl
# 10. Bitext

########## Sentiment Analysis Code ##########
library(ggplot2)
library(syuzhet)
library(tm)
library(qdap) # For rm_stopwords function
########## Read Tweets File ##########
tweets = read.csv(file = "../Desktop/R/Test Files/Sentiment Analysis/trump-tweets-5-4-09-12-5-16/trumptweets1205-127.csv"
                  , header = TRUE
                  , stringsAsFactors = FALSE
                  )

tweet.text = tweets$Tweet
# Writing the tweets into a csv file
# write.csv(  x = tweet.text
#           , file = '../Desktop/R/Test Files/Sentiment Analysis/trump-tweets-5-4-09-12-5-16/tweet_raw_text_r.csv'
#           )

########## Clean Tweets ##########
### Special handling of cleaning tweets using regular expression
# See http://www.endmemo.com/program/R/gsub.php for more regular expression syntax
# Remove "RT", handle etc
tweet.text = gsub(pattern = "(RT|via)((?:\\b\\W*@\\w+)+)"
                  , replacement = ""
                  , x = tweet.text)
# Remove HTML Link
tweet.text = gsub(pattern = "http[^[:blank:]]+"
                  , replacement = ""
                  , x = tweet.text)
# Remove people name
tweet.text = gsub(pattern = "@\\w+"
                  , replacement = ""
                  , x = tweet.text)
# Remove Hashtag - Should be removed before removing punctuation as # is apart of 
# punctuation characters
tweet.text = gsub(pattern = "#\\w+"
                  , replacement = ""
                  , x = tweet.text)
# Replace Punctuations with spaces
tweet.text = gsub(pattern = "[[:punct:]]"
                  , replacement = " "
                  , x = tweet.text)
# Replace alphanumeric characters with space
tweet.text = gsub(pattern = "[^[:alnum:]]"
                  , replacement = " "
                  , x = tweet.text)
# Remove time - digit followed by am/pm with no space between digit and am/pm. E.g. 7PM
# Time should be removed before removing digits
tweet.text = gsub(pattern = "[[:digit:]](am|pm|AM|PM)"
                  , replacement = ""
                  , x = tweet.text)
# Remove digits
tweet.text = gsub(pattern = "[[:digit:]]"
                  , replacement = ""
                  , x = tweet.text)
# Remove whitespaces
tweet.text = gsub(pattern = "(?<=[\\s])\\s*|^\\s+|\\s+$"
                  , replacement = ""
                  , x = tweet.text
                  , perl = TRUE)
# Convert to lower case
tweet.text = sapply(X = tweet.text
                    , FUN = tolower)
# Remove stop words
tweet.text = rm_stopwords(text.var = tweet.text
                          , stopwords = tm::stopwords("english"))
# Removing the stopwords using tm package results in a list. get_nrc_sentiment
# from syuzhet package needs input as a character vector so we have to convert
# the list to character
tweet.text = as.character(tweet.text)

########## Sentiment Analysis ##########
# Sentiment analysis from syuzhet package finds out the score of various
# emotions in the sentence. The columns include one for each emotion type as
# well as a positive or negative score. The ten columns are as follows: 
# 1. anger
# 2. anticipation
# 3. disgust
# 4. fear
# 5. joy
# 6. sadness
# 7. surprise
# 8. trust
# 9. negative
# 10. positive
# The setiment score is found on the word vector and not corpus. The corpus was 
# create the word cloud 
tweet_sentiment = get_nrc_sentiment(char_v = tweet.text)
# head(tweet_sentiment)
#   anger anticipation disgust fear joy sadness surprise trust negative positive
# 1     0            0       0    0   0       0        0     0        0        0
# 2     0            1       0    0   0       1        0     2        1        2
# 3     0            0       0    0   0       0        0     0        0        0
# 4     1            0       0    1   0       1        0     0        2        1
# 5     0            0       0    0   0       0        0     0        0        1
# 6     0            0       0    0   0       0        0     1        0        1
# get_nrc_sentiment() will find out the setiment score of each line of the 
# vector. Each of the line of word vector will result in 10 columns by 
# get_nrc_sentiment(). To get the setiment score of all tweets we have to 
# sum the sentiment score of each column
tweet_sentiment_score = data.frame(colSums(tweet_sentiment[,]))
names(tweet_sentiment_score) = "Score"
# head(tweet_sentiment_score)
#              Score
# anger          114
# anticipation   180
# disgust         83
# fear           110
# joy            139
# sadness        103
# After taking the sum of columns we have to convert this into a dataframe so 
# that the row names of tweet_sentiment_score are also in a column
tweet_sentiment_score = cbind("Sentiment" = row.names(tweet_sentiment_score), tweet_sentiment_score)
# Removing row names
row.names(tweet_sentiment_score) = NULL
########## Plotting the Setiment Scores ##########
ggplot(data = tweet_sentiment_score, aes(x = Sentiment, y = Score)) +
  geom_bar(aes(fill = Sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") +
  ylab("Score") +
  ggtitle("Sentiment of Trump Tweets")
