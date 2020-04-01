if (!require("twitteR")) {
  install.packages("twitteR", repos="http://cran.rstudio.com/") 
  library("twitteR")
}
#google api key - AIzaSyDBCzep1SnddJz4VPw1svsoZsmeCuccKGA
consumerKey<-	"n8QMJkP824hHLYioXmKjomnzo"
consumerSecret<-"9O1iEqVMKRLbgmuhyiurF0tvBEZyjiXbal2CvWSnP9JPG8GF9N"

accessToken<-"1234484240798146560-PsbhlCAdhbBIxkU0JSrUmjXfPdPLdM"
accessSecret<-"ZtniwBsdYVexowqNR9kepDhbdaUC6S4NizU3hAtYW04kr"
#Log your info with the Twitter API:
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)
#######
# Step 2: Download the Followers of a Given Twitter Account
library(rtweet)
search<-search_tweets("joebiden", n=1000)
#######


if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}
#Turn this data into a data frame:
search<-subset(search, location!="")
#######
# Step 4: Geocode Followers' Locations
#######
#Remove special characters:
search$location<-gsub("%", " ",search$location)
#Install key package helpers:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
#Install modified version of the geocode function
#(that now includes the api_key parameter):
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
#Generate specific geocode function:
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDBCzep1SnddJz4VPw1svsoZsmeCuccKGA")
}
#Apply this new function to entire list:
geocode_results<-sapply(search$location, geocode_apply, simplify = F)
#Look at the number of geocoded locations:
length(geocode_results)
#######
print(geocode_results)
#Step 5: Clean Geocoding Results
#######
#Only keep locations with "status" = "ok"
condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_result<-geocode_results[condition_a]
print(geocode_result)

#Only keep locations with one match:
condition_b <- lapply(geocode_result, lapply, length)
condition_b2<-sapply(condition_b, function(x) x["results"]=="1")
geocode_result<-geocode_result[unlist(condition_b2)]
#Look at the number of *successfully* geocoded locations:
length(geocode_results)
#Address formatting issues:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")
#Turn list into a data.frame:
results_b<-lapply(geocode_results, as.data.frame)
results_c<-lapply(results_b,function(x) subset(x, select=c("results.formatted_address", "results.geometry.location")))
#Format thes new data frames:
results_d<-lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                   lat=x[1,"results.geometry.location"],
                                                   lng=x[2,"results.geometry.location"]))
#Bind these data frames together:
results_e<-rbindlist(results_d)
#Add info on the original (i.e. user-provided) locatio/'-n string:
results_f<-results_e[,Original_Location:=names(results_d)]
#Only keep American results:
library(leaflet)
site_locations <- leaflet(results_f) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~results_f,
                   radius = 3, stroke = FALSE)

site_locations

##