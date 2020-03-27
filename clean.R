library(twitteR)
library(ROAuth)
library(dplyr)
library(rtweet)

library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(textstem)
library(plotly)
library(syuzhet)
library(scales)

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

#read csv data from file 
trump_election <- read.csv(file = './trump+election2020.csv')
bernie_election <- read.csv(file = './BernieSanders+election2020.csv')
biden_election <- read.csv(file = './joeBiden+election2020.csv')

#select and clean only columns which is required for analysis
cols <- c("id","created", "screenName", "text", "isRetweet", "retweetCount")

trump_election <- subset(trump_election, select = cols)
trump_election$text <- clean_up_tweets(trump_election$text)

bernie_election <- subset(bernie_election, select = cols)
bernie_election$text <- clean_up_tweets(bernie_election$text)

biden_election <- subset(biden_election, select = cols)
biden_election$text <- clean_up_tweets(biden_election$text)



#get only tweets for Emotion detection 
tweets_only_trump = trump_election$text
tweets_only_biden = biden_election$text
tweets_only_bernie = bernie_election$text

get_emotions <- function(tweets){
  emotions<- get_nrc_sentiment(tweets)
  emo_bar = colSums(emotions)
  emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
  emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
  emo_sum <- emo_sum[1:8,]
  emo_sum$percent<-(emo_sum$count/sum(emo_sum$count))*100
  
  return (emo_sum)
}

trump_emotions <- get_emotions(tweets_only_trump$text)
bernie_emotions <- get_emotions(tweets_only_bernie$text)
biden_emotions <- get_emotions(tweets_only_biden$text)

# #Visualize the emotions from NRC sentiments
fig1 <- plot_ly(trump_emotions, x=~emotion, y=~percent, type="bar", color=~emotion) %>%
  layout(xaxis=list(title="Trump"),  yaxis = list(title = "Emotion count"),
         showlegend=FALSE,title="Distribution of emotion categories") %>%
  layout(yaxis = list(ticksuffix = "%"))

fig2 <- plot_ly(bernie_emotions, x=~emotion, y=~percent, type="bar", color=~emotion) %>%
  layout(xaxis=list(title="Bernie"),  yaxis = list(title = "Emotion count"),
         showlegend=FALSE,title="Distribution of emotion categories") %>%
  layout(yaxis = list(ticksuffix = "%"))

fig3 <- plot_ly(biden_emotions, x=~emotion, y=~percent, type="bar", color=~emotion) %>%
  layout(xaxis=list(title="Biden"),  yaxis = list(title = "Emotion count"),
         showlegend=FALSE,title="Distribution of emotion categories") %>%
  layout(yaxis = list(ticksuffix = "%"))

subplot(fig1,fig2,fig3)

#Detecting Sentiment Polarity
#Reading the Lexicon positive and negative words
pos <- readLines("./positive_words.txt")
neg <- readLines("./negative_words.txt")

#function to calculate sentiment score
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    # remove punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence <- gsub('\\d+', '', sentence)
                    
                    #convert to lower
                    sentence <- tolower(sentence)
                    
                    
                    # split sentence into words with str_split (stringr package)
                    word.list <- str_split(sentence, "\\s+")
                    words <- unlist(word.list)
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos)
                    neg.matches <- match(words, neg)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}
#sentiment score
scores_twitter <- score.sentiment(tweets_only_bernie, pos.txt, neg.txt, .progress='text')


View(scores_twitter)

#Summary of the sentiment scores
summary(scores_twitter)

scores_twitter$score_chr <- ifelse(scores_twitter$score < 0,'Negtive', ifelse(scores_twitter$score > 0, 'Positive', 'Neutral'))


View(scores_twitter)


#Convert score_chr to factor for visualizations
scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)
names(scores_twitter)[3]<-paste("Sentiment")  

#plot to show number of negative, positive and neutral comments
Viz1 <- ggplot(scores_twitter, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
Viz1


