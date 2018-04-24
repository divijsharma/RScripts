########## Introduction to WordCloud ##########
# Libraries for Word Cloud
# Text Mining library
library(NLP)
library(tm)
# Word stemming algorithm for collapsing words to a common root to aid 
# comparison of vocabulary. Converting Walking, Walked to Walk is called 
# stemming. 
library(SnowballC)
# Library for creating word cloud
library(RColorBrewer)
library(wordcloud)
library(wordcloud2) # Wordcloud2 is a better packages that has much more flexibility on
# shape of wordcloud. See also https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html

########## Read data ##########
# By choosing stringsAsFactors = FALSE the text is read as text and not 
# converted to factors. The default option is stringsAsFactors = TRUE because
# of which the text is converted to factors and it becomes difficult to 
# manipulate the data
input_data <- read.csv(file = "C:\\Users\\dsharma\\Desktop\\R\\Test Files\\JEOPARDY_CSV.csv", header = TRUE, stringsAsFactors = FALSE)

########## Create Corpus and clean data ##########
### FOR TWITTER SPECIFIC CLEANING SEE - SENTIMENT ANALYSIS.R 
### https://github.com/divijsharma/RScripts/blob/master/Sentiment%20Analysis.R
# Whole of data mining works on Corpus which is an abstract concept representing
# the a collection of text documents.  The default implementation is
# VCorpus (short for Volatile Corpus) which caters to most R objects
# Corpora are R objects held fully in memory. So essentially the below 
# definition of Corpus creates a VCorpus. To create a Corpus the first argument
# is a source.
# VectorSource defines the source as the character vector. VectorSource converts
# each word of the Question column of the file into a vector
input_corpus <- Corpus(VectorSource(input_data$Question))

# tm_map is the main function to do data cleaning. In the below statement
# Corpus is converted to Plain text.
# input_corpus <- tm_map(input_corpus, PlainTextDocument)
# Reason for commenting this line of code is given after the code for creating TDM

# Remove unnecessary white spaces
input_corpus <- tm_map(input_corpus, stripWhitespace)

# Removing specific words from the corpus. Stop words are removed when creating the TermDocumentMatrix
# below. Removing the words is case sensitive.
# The and This are not required if we have already converted everything to lower case. As we are converting
# to lower case while creating TermDocumentMatrix below we have included The and This
input_corpus <- tm_map(input_corpus, removeWords, c("the", "this", "The", "This"))

########## Create Term Document Matrix ##########
# TDM has the words and the frequecy of each
# In the below code only the words with length >= 3 are considered. The 
# wordLengths=c(3,Inf) takes of that.
tdm <- TermDocumentMatrix(input_corpus, 
                          control = list(removeNumbers = TRUE
                                         , stopwords = TRUE
                                         , tolower = TRUE
                                         , removePunctuation = TRUE
                                         , wordLengths=c(3,Inf)
                                         #, stemming = TRUE
                                        )
                          )
### Error while creating TDM ###
### Error in simple_triplet_matrix(i, j, v, nrow = length(terms), ncol = length(corpus),  : 
### 'i, j' invalid
# To solve this error the corpus should not be converted to PlainTextDocument by 
# commenting input_corpus <- tm_map(input_corpus, PlainTextDocument)

#### The below code is commented as everything can be done while creating TDM.
#### This is just a placeholder to show alternate approach. 
# # tm_map is the main function to do data cleaning. In the below statement
# # Corpus is converted to Plain text.
# input_corpus <- tm_map(input_corpus, PlainTextDocument)
# 
# # Remove unnecessary white spaces
# input_corpus <- tm_map(input_corpus, stripWhitespace)
# 
# # Convert everything to lower case. Content_trasformer is required otherwise we get
# # an error in creating TextDocumentMatrix
# input_corpus <- tm_map(input_corpus, content_transformer(tolower))
# 
# # Remove numbers
# input_corpus <- tm_map(input_corpus, removeNumbers)
# 
# # All the punctuations marks are removed.
# input_corpus <- tm_map(input_corpus, removePunctuation)
# 
# # All the stopwords are removed alongwith the and this. Removing the words is case sensitive. 
# # The and This are not required if we have already converted everything to lower case
# input_corpus <- tm_map(input_corpus, removeWords, c("the", "this", "The", "This", stopwords('english')))
# 
# # The words are stemmed so that various forms of the word are considered only 
# # once. For example - Walk, Walked, Walking will be stemmed to Walk. Y ending words are stemmed
# # to i ending words. City is stemmed to Citi, Only is stemmed to Onli. I don't know whi. :)
# input_corpus <- tm_map(input_corpus, stemDocument)
# 
# ##### !!!!!! pal <- brewer.pal(9, "BuGn")
# ##### !!!!!! pal <- pal[-(1:2)]
# ##### !!!!!! wordcloud(jeopCorpus, min.freq = 3, max.words = 100, random.order = FALSE, ordered.colors = TRUE, colors = pal )
# 
# # The error -
# # In wordcloud(...): <word> could not be fit on page. It will not be plotted.
# # can be solved by reducing the scale parameter. I introduced the scale = c(4,0.2) 
# # parameter. Scale controls the difference between largest and smallest font. 
# # rot.per is the percentage of vertical text. 
# ##### !!!!!! Color is giving some errors
# ##### !!!!!!
# ##### !!!!!!   Length of colors does not match length of words vector
# ##### !!!!!!
# ##### !!!!!! The below code is giving issues. So I am replcing the 
# ##### !!!!!! code with new code. I still don't know why the below code
# ##### !!!!!! is giving issues. 
# ##### !!!!!! pal <- brewer.pal(9, "BuGn")
# ##### !!!!!! pal <- pal[-(1:2)]
# ##### !!!!!! wordcloud(jeopCorpus, min.freq = 3, max.words = 100, random.order = FALSE, ordered.colors = TRUE, colors = pal )
# 
# ##### !!!!!! The below code was written to solve the color error.
# # Create a Term Document Matrix. A document-term matrix or term-document matrix 
# # is a mathematical matrix that describes the frequency of terms that occur in 
# # a collection of documents. In a document-term matrix, rows correspond to 
# # documents in the collection and columns correspond to terms. Each row will be
# # one word. Because of wordlengths only words with >= 3 characters will
# # be in TDM
# tdm <-TermDocumentMatrix(input_corpus, control=list(wordLengths=c(3,Inf)))

########## Separate the word and frequencies ##########
frequency_of_words <- slam::row_sums(tdm)
individual_words <- names(frequency_of_words)

# Create a data frame with words and frequencies
word_data_frame <- data.frame(individual_words,frequency_of_words)
colnames(word_data_frame) = c("Words", "Frequency")
rownames(word_data_frame) = NULL

# Sorting the data frame on descending order of frequency to understand which words
# occur more requently. Based on top 100 frequencies, I will assign the color. Please note 
# that the logic to assign final_color requires manual coding depending on the frequency
word_data_frame <- word_data_frame[rev(order(word_data_frame$Frequency)),]

########## Generate the word cloud ########## 
# Defining the tone depending on the frequency of occurance of the word
# tone <- (word_data_frame$frequency_of_words - min(word_data_frame$frequency_of_words)) / (max(word_data_frame$frequency_of_words) - min(word_data_frame$frequency_of_words))

# final_color <- rgb(tone,0,0)
# Assigning the colour of the word. The logic used is as follows - 
# Frequency         Color
# 0-300             Green
# 300-600           Blue
# 600-900           Red

# for (i in 1:NROW(word_data_frame)) {
#   if (word_data_frame$frequency_of_words[i] > 0 & word_data_frame$frequency_of_words[i] <= 300){
#     final_color <- rgb(0, 255, 0, maxColorValue = 255)
#     final_color[i]
#   }
#   if (word_data_frame$frequency_of_words[i] > 300 & word_data_frame$frequency_of_words[i] <= 600){
#     final_color <- rgb(0, 0, 255, maxColorValue = 255)
#   }
#   if (word_data_frame$frequency_of_words[i] > 600 & word_data_frame$frequency_of_words[i] <= 900){
#     final_color <- rgb(255, 0, 0, maxColorValue = 255)
#   } 
#   # else
#   # {
#   #   final_color <- rgb(255,255,255)
#   # }  
# }

# wordcloud(individual_words, frequency_of_words,  min.freq = 100, scale = c(4,0.5), max.words = 100, color=final_color, ordered.colors=TRUE, random.color=FALSE)

# Using the png command does not show the wordcloud in Plots area. The png is saved in current
# working directory. If dev.off() is not coded then png is not generated unless we close RStudio.
png("WordCloud.png", width=12, height=8, units="in", res=300)
# This wordcloud is generated from wordcloud package
wordcloud(words = word_data_frame$Words
          , freq = word_data_frame$Frequency
          , min.freq = 100
          , scale = c(5, 0.5) # Height of max frequency word = 5 and lowest = 0.5
          , max.words = 100
          , color=brewer.pal(8, "Dark2")
          , random.order = FALSE
          , rot.per = 0.6 # 60% of words will be rotated
          )
dev.off()

########## Generating Word Cloud using Wordcloud2 package ##########
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
# While wordcloud2 gives a lot of flexibility on shape of wordcloud, there are a
# few issues 
# 1. There is no parameter to restrict the number of words in the wordcloud2; 
#    similar to max.words parameter. Due to lack of max.words parameter, the 
#    wordcloud takes a lot of time to generate also. 
# 2. The wordcloud generated is not a simple plot but an interactive display that 
#    displays the frequency of the word highlighted by moving cursor hence the 
#    display can't be easily converted to png.

# In order to circumvent the first issue - create a new data frame with first 100
# rows of word_data_frame. As word_data_frame is already sorted in descending order
# of frequency taking first 100 rows will result in wordcloud of top 100 words
wordcloud2_data_frame = word_data_frame[1:100, ]
# In order to circumvent the second issue - use webshot and htmlwidgets library. 
# The widget from wordcloud2 is first stored in an html file in the working 
# directory using htmlwidgets::saveWidget and then converted to png by taking 
# screenshot of generated html using webshot::webshot
library(webshot)
library(htmlwidgets)
webshot::install_phantomjs()
wc = wordcloud2(data = wordcloud2_data_frame
           , size = 0.8
           , shape = 'circle'
           , rotateRatio = 0.5
           , minSize = 1
)
saveWidget(widget = wc
           , file = "wc.html"
           , selfcontained = FALSE)
webshot(url = "wc.html"
        , file = "wc2.png"
        , vwidth = 1992
        , vheight = 1744
        , delay = 10
        )

#

# Lettercloud has issues in displaying to RStudio viewer. It is an open issue.
# https://github.com/ramnathv/htmlwidgets/issues/262
lc = letterCloud(data = word_data_frame
            , word = 'Test'
            , wordSize = 2)
saveWidget(widget = lc
           , file = "lc.html"
           , selfcontained = FALSE)
webshot(url = "lc.html"
        , file = "wc2_lc.png"
        , vwidth = 1992
        , vheight = 1744
        , delay = 10
        )
# The above is also not working. It is showing blank image. Need to check on that. 
