# fetch tweets data from twitter search API, 
# extract URLs, resolve shortened urls to real urls, 
# open each in browser
library(twitteR)
library(lubridate)
library(tm)
#library(Hmisc)
setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter/")
##Authenticate here to query the twitter API##
###You need to create an app at https://apps.twitter.com/app/ ###
api_key   =        Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")
access_token            = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret     = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

rate.limit <- getCurRateLimitInfo()
# print out all metrics that have been changed from their default values
rate.limit[rate.limit$limit != rate.limit$remaining,]

#Search twitter for 1000 tweets containing the word 'Cluster Analysis'
#tweets <- searchTwitter("#FCBFCB",n=1000)

#
#
#
# Perform query
#
# geocode:52.5226762,13.3790944,50mi
#
days_back <- 8
#query.job <- paste0("#Potsdam (#job OR #jobs OR #stellenangebot) -RT since:" , format(now() - days(days_back), "%Y%m%d"))
query.job <- paste0("#Hamburg (#job OR #jobs OR #stellenangebot) -RT since:" , format(now() - days(days_back), "%Y%m%d"))
#query.job <- paste0("#Berlin (#job OR #jobs OR #stellenangebot) -RT since:" , format(now() - days(days_back), "%Y%m%d"))

query.name <- "hamburgjobs"
query.name.table <- paste0(query.name, "_status")
#
#query <- paste0("#Potsdam -RT since:" , format(now() - days(days_back), "%Y%m%d"))
tweets <- searchTwitter(query.job,n=1000)

# store inside a database, 
register_sqlite_backend(paste0("tweets_jobsearch.sqlite"))
store_tweets_db(tweets,table_name = query.name.table)
#store_users_db(tweets,table_name = paste0(query.name, "_users"))
tweets.from_db = load_tweets_db(as.data.frame = TRUE, table_name = query.name.table)

tweets.from_db$longitude <- as.double(tweets.from_db$longitude)
tweets.from_db$latitude <- as.double(tweets.from_db$latitude)
tweets.df<-unique(tweets.from_db[tweets.from_db$longitude > 0 | is.na(tweets.from_db$longitude),])

#print as YAML
ppy(tweets.df[1:2,])

scrnames <- tapply(tweets.df$text, tweets.df$screenName, length)
# topusernames posting jobs
scrnames[names(scrnames[order(scrnames, decreasing = TRUE)])[1:10]]

# filter results
tweets.df.text <- tweets.df[grep("data|soft", tweets.df$text, perl=TRUE, ignore.case = TRUE), "text"]
#build corpus
myCorpus <- Corpus(VectorSource(as.vector(tweets.df.text)))
shown <- 15
shown <- min(length(myCorpus), shown)
randn <- sample(length(myCorpus), shown)

# convert difficult-to-handle international characters
myCorpus <- tm_map(myCorpus, function(x) {
        doc <- tryCatch({
                iconv(enc2utf8(content(x)), sub = "byte")
        },error=function(e){warning(e); return("cannot convert text")
        })
        PlainTextDocument(doc)
})
myCorpus
sapply(myCorpus[1], meta)
sapply(myCorpus[randn], function(x){ppy(content(x))})

# most urls are anonymous t.co urls, created by twitter, example:
extractURL(content(myCorpus[[1]]))

# put original urls into metadata section of the tm corpus
myCorpus <- tm_map(myCorpus, function(x){meta(x)["x.uris"] <- extractURL(content(x)); x})
sapply(myCorpus[1], meta)

sapply(myCorpus[length(myCorpus)], meta)
#URLs <- lapply(myCorpus, function(x){c(meta(x)["id"], meta(x)["x.uris"])})
URLs <- sapply(myCorpus, function(x){ meta(x)["x.uris"]})
(URLs <- unlist(URLs))
n <- 2 #shown
idx <- sample(length(URLs), n)
meta(myCorpus[[1]])
sapply(myCorpus, function(x){
        meta(x)["x.uris"]
})
# unshorten urls, add them to metadata section
myCorpus <-  tm_map(myCorpus, function(x){
        url <- ""
        if(! (meta(x)["x.uris"] == "character(0)") & nchar(unlist(meta(x)["x.uris"]) > 0)){
                url <-try(  {
                        unshorten_url(meta(x)["x.uris"][[1]])
                });
                meta(x)["x.uris.resolved"] <- ifelse(nchar(url) > 0, url, "")
        }
        x
})

# resolve URLs, time consuming.
(URLS_unshortened <-  lapply(myCorpus, function(x) {c(content(x), meta(x)[["x.uris.resolved"]])}))
#URLS_unshortened <- unique(grep("youtube",URLS_unshortened, value = TRUE, invert = TRUE))


# open urls in browser
(res <- sapply(unique(URLS_unshortened[1]), function(x){
        printf(x[[1]])
        system(paste0("firefox ", x[[2]]))  
}))
