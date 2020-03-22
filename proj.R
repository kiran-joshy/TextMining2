library(twitteR)
library(ROAuth)

# authenticate with Twitter

consumerKey<-	"n8QMJkP824hHLYioXmKjomnzo"
consumerSecret<-"9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"

accessToken<-"1234484240798146560-PsbhlCAdhbBIxkU0JSrUmjXfPdPLdM"
accessSecret<-"ZtniwBsdYVexowqNR9kepDhbdaUC6S4NizU3hAtYW04kr"

## 2. Retrieval of timelines

##You can retrieve a user's timeline (up to a maximum of 3200 tweets). Install rtweet and provide the authentication keys from Twitter. Use the get_timeline(s) functions to retrieve tweets that have been posted by a user. Specify the number of tweets you want to extract, then save the output to your desired format.

library(rtweet)

# insert the consumer key and consumer secret from twitter
create_token(
  consumer_key = "n8QMJkP824hHLYioXmKjomnzo",
  consumer_secret = "9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"
)
# replace with the target user screen name or ID
Trump_timeline<-get_timeline("realDonaldTrump",n=700)
Bernie_timeline<-get_timeline("BernieSanders",n=700)
Joe_timeline<-get_timeline("JoeBiden",n=700)

# only get the data after 1 Jan 2019
#Trump_timeline <- Trump_timeline[1:677,]
#Bernie_timeline <- Bernie_timeline[1:295,]

# save to .RData file (files can be found in the attchments)
save(Trump_timeline, file = "Trump_timeline.RData")
save(Bernie_timeline, file = "Bernie_timeline.RData")
save(Joe_timeline, file = "Joe_timeline.RData")

## 3. Retrieving the followers of a specific user

##To retrieve a list of IDs following a specific user, use the code below.

library(rtweet)

# insert the consumer key and consumer secret from twitter
create_token(
  consumer_key = "n8QMJkP824hHLYioXmKjomnzo",
  consumer_secret = "9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"
)

# get a list IDs of followers; replace with target user screen name
Trump_followers<- get_followers("realDonaldTrump", n = 5000, page = "-1", retryonratelimit = FALSE,
                          parse = TRUE, verbose = TRUE, token = NULL) 
Bernie_followers<- get_followers("BernieSanders", n = 5000, page = "-1", retryonratelimit = FALSE,
                                parse = TRUE, verbose = TRUE, token = NULL)
Joe_followers<- get_followers("JoeBiden", n = 5000, page = "-1", retryonratelimit = FALSE,
                                 parse = TRUE, verbose = TRUE, token = NULL)

# count number of followers
Trump_count<-nrow(Trump_followers)
Bernie_count<-nrow(Bernie_followers)
Joe_count<-nrow(Joe_followers)

#write it to file
write.csv(Trump_followers, file="Trump_followers.csv")
write.csv(Bernie_followers, file="Bernie_followers.csv")
write.csv(Joe_followers, file="Joe_followers.csv")

## 4. Retrieving a list of accounts (with associated metadata) followed by a user

##To retrieve a list of IDs of accounts followed by a user use the code below. In addition, using the user ID, you can retrieve metadata associated with these accounts such as name, location, language, etc. The result will be a data frame with 88 metadata fields.

library(rtweet)

# insert the consumer key and consumer secret from twitter
create_token(
  consumer_key = "n8QMJkP824hHLYioXmKjomnzo",
  consumer_secret = "9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"
)

# get list of accounts followed by a given account; replace with user screen name of interest
Trump_fds <- get_friends("realDonaldTrump")
Bernie_fds <- get_friends("BernieSanders")
Joe_fds <- get_friends("JoeBiden")

# save result as a CSV file
write.csv(Trump_fds, file="Trumpfds.csv") 
write.csv(Bernie_fds, file="Berniefds.csv")
write.csv(Joe_fds, file="Joefds.csv")

# retrieve metadata for these accounts: name, location, language, etc.
Trump_fds_data <- lookup_users(Trump_fds$user_id)
write.csv(Trump_fds_data, file="Trumpfds_metadata.csv") 

Bernie_fds_data <- lookup_users(Bernie_fds$user_id)
write.csv(Bernie_fds_data, file="Berniefds_metadata.csv") 

Joe_fds_data <- lookup_users(Joe_fds$user_id)
write.csv(Joe_fds_data, file="Joefds_metadata.csv") 

## 5.  Visualising a user's retweet network

##This code will extract retweets from a user's timeline and display them as a network.
library(twitteR)
library(ROAuth)
library(igraph)
library(stringr)

consumerKey<-	"n8QMJkP824hHLYioXmKjomnzo"
consumerSecret<-"9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"
accessToken<-"1234484240798146560-PsbhlCAdhbBIxkU0JSrUmjXfPdPLdM"
accessSecret<-"ZtniwBsdYVexowqNR9kepDhbdaUC6S4NizU3hAtYW04kr"

# authenticate
setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessSecret)  

# replace with the target user screen name or ID (Trump)
dm_tweets = userTimeline("realDonaldTrump", n=300, includeRts = TRUE) 

# get text
dm_txt = sapply(dm_tweets, function(x) x$getText())

# regular expressions to find retweets
grep("(RT|via)((?:\\b\\W*@\\w+)+)", dm_tweets, ignore.case=TRUE, value=TRUE)

# determine which tweets are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", dm_txt, ignore.case=TRUE)

# show retweets (these are the ones we want to focus on)
dm_txt[rt_patterns] 

# create list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

for (i in 1:length(rt_patterns))
{ 
# get tweet with retweet entity
twit = dm_tweets[[rt_patterns[i]]]

# get retweet source 
poster = str_extract_all(twit$getText(), "(RT|via)((?:\\b\\W*@\\w+)+)") 

# remove ':'
poster = gsub(":", "", unlist(poster)) 

# name of retweeted user
who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 

# name of retweeting user 
who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}

# unlist
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

# create graph from an edge list
# results in two column matrix of edges
retweeter_poster = cbind(who_retweet, who_post)

# generate the graph
rt_graph = graph.edgelist(retweeter_poster)

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

# choose a layout
glay = layout.fruchterman.reingold(rt_graph)

# plot
par(bg="gray1", mar=c(1,1,1,1))
result <- plot(rt_graph, layout=glay,
vertex.color="gray25",
vertex.size=10,
vertex.label=ver_labs,
vertex.label.family="sans",
vertex.shape="none",
vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
vertex.label.cex=0.85,
edge.arrow.size=0.8,
edge.arrow.width=0.5,
edge.width=3,
edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))

# add title
title("Interacting accounts with realDonaldTrump official twitter account: Retweets",
cex.main=1, col.main="gray95")


#{r echo=FALSE, out.width="50%", fig.cap="retweet network for Ofgem",fig.align= "center",message=FALSE, warning=FALSE}

# for knitting the document and enabling the include_graphics function
library(knitr)    
library(png)

# for grabbing the dimensions of png files
include_graphics("retweet_network.png")
