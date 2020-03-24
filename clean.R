library(twitteR)
library(ROAuth)
library(dplyr)
library(rtweet)

library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(textstem)

# authenticate with Twitter
consumerKey<-	"n8QMJkP824hHLYioXmKjomnzo"
consumerSecret<-"9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"

accessToken<-"1234484240798146560-PsbhlCAdhbBIxkU0JSrUmjXfPdPLdM"
accessSecret<-"ZtniwBsdYVexowqNR9kepDhbdaUC6S4NizU3hAtYW04kr"

setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessSecret)

#Read in tweets TO candidates and then convert to Dataframe
tweets_to_trump <- searchTwitter("@realDonaldTrump", n=1000, retryOnRateLimit=120)
tweets_to_trump <- bind_rows(lapply(tweets_to_trump, as.data.frame))

#Read in tweets ABOUT candidates and then convert to Dataframe
tweets_about_trump <- searchTwitter("#realDonaldTrump", n=1000, retryOnRateLimit=120)
tweets_about_trump <- bind_rows(lapply(tweets_about_trump, as.data.frame))

write.csv(tweets_to_trump, file="./tweets_to_trump.csv") 
write.csv(tweets_about_trump, file="./tweets_about_trump.csv") 


clean_up_tweets <- function(tweets) {
  
  # convert text to lowercase
  tweets <- tolower(tweets)
  
  # get rid of problem characters
  tweets <- sapply(tweets,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  # remove rt
  tweets <- gsub("rt", "", tweets)
  
  # remove punctuation, digits, special characters etc
  tweets = gsub("&amp", "", tweets)
  
  tweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
  tweets = gsub("@\\w+", "", tweets)
  tweets = gsub("[[:punct:]]", "", tweets)
  tweets = gsub("[[:digit:]]", "", tweets)
  tweets = gsub("http\\w+", "", tweets)
  tweets = gsub("[ \t]{2,}", "", tweets)
  tweets = gsub("^\\s+|\\s+$", "", tweets)
  
  # get rid of unnecessary spaces
  tweets <- str_replace_all(tweets," "," ")
  
  # take out the retweet header (there is only one)
  tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
  
  # get rid of references to other screen names
  tweets <- str_replace(tweets,"@[a-z,A-Z]*: ","")
  
  # get rid of hashtags
  tweets <- str_replace(tweets,"#[a-z,A-Z]*: ","")
  
  # get rid of blank space at the end of the tweet
  tweets <- gsub(" $", "", tweets)
  
  #convert list of tweets to corpus
  tweets_corpus <- Corpus(VectorSource(tweets))
  
  #remove stop words
  tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english"))
  
  #strip whitespace
  tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)
  
  #lemmatize words in corpus
  tweets_corpus <- tm_map(tweets_corpus, lemmatize_strings)
  
  #convert corpus back into dataframe
  return(data.frame(text_clean = get("content", tweets_corpus), 
                    stringsAsFactors = FALSE))
}

tweets_to_trump$text <- clean_up_tweets(tweets_to_trump$text)











