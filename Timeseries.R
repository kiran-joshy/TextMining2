install.packages("slam")
library("slam")
install.packages("tm")
library("tm")
library("lubridate")
library("syuzhet")
library("dplyr")
install.packages("reshape2")
library("reshape2")
library("ggplot2")
library("tmap")
install.packages(c("stringi", "stringr", "reshape2", "rattle"), dependencies = TRUE) 

#read in the data for diff people
textdata <- read.csv("Joebiden+election2020-17k.csv", encoding = "UTF-8")

#convert the twitter data format
textdata$created <- as.POSIXct(textdata$created_at, format="%Y-%m-%d %H:%M:%S") #2020-03-28 01:57:42
#select the months and keep as a date format
textdata$Month <- format(as.Date(textdata$created), "%m")
textdata$Month2 <- months(textdata$created)

names(textdata)[names(textdata) == 'Text'] <- 'text'#incase you data has the column name #Text, change it to "text" as the next line will only accept the column name "text"
#take the text column and convert to a corpus
textdata$doc_id<-textdata$doc_id <- seq_len(nrow(textdata))  # include the doc_id
#text<as.character(textdata$text)
corpus <- Corpus(DataframeSource(textdata))
corpus <- Corpus(DataframeSource(textdata))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

#form a document term matrix
DTM <- DocumentTermMatrix(corpus)
 ## You might consider using cbind.data.frame instead of cbind

#select the terms you want to observe
terms_to_observe <- c( "good", "joe") ## just for example 
#terms_to_observe <- c("nation", "war", "god", "terror", "security")

#reduce the DTM to contain only those terms
DTM_reduced <- as.matrix(DTM[, terms_to_observe])
#sum the frequecies 
counts_per_month<- aggregate(DTM_reduced, by = list(decade = textdata$Month), sum)



counts_per_month_long <- melt(counts_per_month, id="decade")  # convert to long format

#Visualize the word frequecy time series
p2 <- ggplot(data = counts_per_month_long, aes(x = factor(decade), y = value, colour = variable)) +       
  geom_line(aes(group = variable)) + geom_point() + xlab("Month") +
  ylab("Frequency") +  labs(color='Terms to observe') 

#showing the plot
p2