# fetch twitter data, clean tweets, remove URLs
# create wordcloud,
# perform cluster analysis,
# extract user info who tweeted about this
library(twitteR)
library(lubridate) # days back
library(tm) 
library(wordcloud)
#library(dplyr) # 
setwd("/home/knut/code/git/_my/tweets2sqlite")
source("twitterUtils.R")
source("tmUtils.R")
##Authenticate here to query the twitter API##
###You need to create an app at https://apps.twitter.com/app/ ###
#sysenv_search("TW_APP")
#sysenv_search("TW_APP_RCMDR")
api_key   =        Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")
access_token            = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret     = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")
options(httr_oauth_cache=TRUE)
        setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#1
ratelimits()
#Search twitter for 1000 tweets containing the word 'Cluster Analysis'
#tweets <- searchTwitter("#FCBFCB",n=1000)

#
#
#
# Perform query
#
# geocode:52.5226762,13.3790944,50mi
#
is_stored <- FALSE
days_back <- 1
(date_back <- format(now() - days(days_back), "%Y-%m-%d"))
days_until <- 0
(date_until <- format(now() - days(days_until), "%Y-%m-%d"))
(query <- paste0("#MSC2016 -RT since:" , date_back, " until:",date_until))
        
query.name <- "qry_munsecconf"
table.name <- query.name
tweets <- searchTwitter(query,n=2000)
# store inside a database, 
db.name <- paste0("db/tweets_allkindsof.sqlite")
register_sqlite_backend(db.name)

#store one tweet to check if sqlite is available
is_stored <- store_tweets_db(tweets[1],table_name = query.name)
#is_stored <- FALSE
if (is_stored == TRUE){
        print(paste0("storing tweets in table ", table.name))
        is_stored2 <- store_tweets_db(tweets,table_name = table.name)
        makeTweetsTableUnique(db.name = db.name, table.name=table.name)
        tweets.df = load_tweets_db(as.data.frame = TRUE, table_name = table.name)
} else {
        tweets.df <- unique(twitteR::twListToDF(tweets))
}

# show a few tweets
N <- 10
ntw <- sample(nrow(tweets.df), N)
t(tweets.df[ntw, c("screenName","text")])

