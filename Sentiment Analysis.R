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
###
# Steps - 
# 1. Read data
# 2. Clean data
# 3. Create list of Positive words
# 4. Create list of Negative words
# 5. Apply Sentiment Algorithm
# 6. Analyze the result - Frequency Distribution, Mean, Median, Histogram etc.
###

########## Sentiment Analysis Code ##########
####
## Based on the video - https://www.youtube.com/watch?v=ZepOMlAjuB4
####
library(ggplot2)
library(wordcloud)
library(sentiment)
library(syuzhet)
library(tm)
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

########## Creating Word Corpus and Cleaning data ##########
# Corpus is kind of a big bag of words
tweet_corpus = Corpus(VectorSource(tweet.text))
# tm_map is the main function to do data cleaning of a corpus. 
# Converting to plain text
tweet_corpus = tm_map(tweet_corpus, PlainTextDocument)
# Removing puntuation
tweet_corpus = tm_map(tweet_corpus, removePunctuation)
# Converting to lower case
tweet_corpus = tm_map(tweet_corpus, content_transformer(tolower))
# Removing stopwords
tweet_corpus = tm_map(tweet_corpus, removeWords, stopwords("english"))
# Removing whitespaces
tweet_corpus = tm_map(tweet_corpus, stripWhitespace)
# Stem words
tweet_corpus = tm_map(tweet_corpus, stemDocument)
########## Creating Wordcloud ##########
pal = brewer.pal(n = 8
                 , name = "Dark2")

wordcloud(words = tweet_corpus
          , min.freq = 5
          , max.words = Inf
          , random.order = FALSE
          , colors = pal)

########## Sentiment Analysis ##########
# Sentiment analysis from syuzhet pacakes finds out the score of various
# emotions in the sentence. The columns include one for each emotion type as
# well as a positive or negative valence. The ten columns are as follows: 
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
# The setiment score is found on the word vector and not corpus
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
tweet_sentiment_scroe = data.frame(colSums(tweet_sentiment[,]))
names(tweet_sentiment_scroe) = "Score"
# head(tweet_sentiment_scroe)
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
