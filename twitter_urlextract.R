# fetch tweets data from twitter search API, 
# extract URLs, resolve shortened urls to real urls, 
# open each in browser
library(twitteR)
library(lubridate) # date arrithmetics
library(tm) # used in second half
source("twitterUtils.R")
source("tmUtils.R")
#library(Hmisc)
setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter/")
##Authenticate here to query the twitter API##
###You need to create an app at https://apps.twitter.com/app/ ###
api_key   =        Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")
access_token            = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret     = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


ratelimits()

#
# Perform query
#
# geocode:52.5226762,13.3790944,50mi
#
days_back <- 3
(date_back <- format(now() - days(days_back), "%Y-%m-%d"))
days_until <- 0
(date_until <- format(now() - days(days_until), "%Y-%m-%d"))
(query.job <- paste0("#Hamburg (#job OR #jobs OR #stellenangebot) -RT since:" , date_back, " until:",date_until))

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

# only take tweets with geocoordinates
tweets.from_db$longitude <- as.double(tweets.from_db$longitude)
tweets.from_db$latitude <- as.double(tweets.from_db$latitude)
tweets.df <- unique(tweets.from_db[tweets.from_db$longitude > 0 | is.na(tweets.from_db$longitude),])

#print as YAML
ppy(tweets.df[1:2,])

# top usernames posting (about) jobs
scrnames <- tapply(tweets.df$text, tweets.df$screenName, length)
(scrnames[names(scrnames[order(scrnames, decreasing = TRUE)])[1:10]])

# filter results
tweets.df <- tweets.df[grepl("data|soft", tweets.df$text, perl=TRUE, ignore.case = TRUE),]
#build corpus, simple way
#myCorpus <- Corpus(VectorSource(as.vector(tweets.df.text)))
myCorpus <- DataframeSource(tweets.df)
myCorpus <- Corpus(myCorpus,
                   readerControl = list(reader=commonReader()))
shown <- 15
shown <- min(length(myCorpus), shown)
randn <- sample(length(myCorpus), shown)
myCorpus <- tm_map(myCorpus, content_transformer(tm_convertToUTF8))

myStopwords <- c("<ed><ae><ba><ed><be><85")
myCorpus <- tm_map(myCorpus, content_transformer(tm_removeStopwords), myStopwords)

# first 3 words become 'heading' metadata entity
myCorpus <- tm_map(myCorpus, setHeading)

#myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus <- tm_map(myCorpus, content_transformer(tm_removeNonAlnum))
#myCorpus <- tm_map(myCorpus, content_transformer(tolower))
tryCatch({sapply(randn, function(i) {content(myCorpus[[i]])})}, error=function(e){warning(e); return("cannot show content:")})


# 'id' metadata entity becomes 'description' plus 'screenName' - easier for DTMs
tryCatch({myCorpus <- tm_map(myCorpus, setId)}, error=function(e){warning(e); return("cannot set heading:")})
# add a humanreadable datestring
myCorpus <- tm_map(myCorpus, setDatestr)


myCorpus
sapply(myCorpus[1], meta)
res <- sapply(myCorpus[randn], function(x){ppy(content(x))})

myStopwords <- c(stopwords("english"), stopwords("german"), "rt", "@", "-", "via")
myCorpus <- tm_map(myCorpus, content_transformer(tm_removeStopwords), myStopwords)
sapply(randn, function(i) {meta(myCorpus[[i]])})

# # only retweeted multiple times
# myCorpus <- tm_filter(myCorpus, function(x){
#         meta(x, tag="retweetCount") > 0
# })

# most urls are anonymous t.co urls, created by twitter, example:
extractURL(content(myCorpus[[1]]))

# put original urls into metadata section of the tm corpus
myCorpus <- tm_map(myCorpus, function(x){meta(x)["x.uris"] <- extractURL(content(x)); x})
sapply(myCorpus[1], meta)

sapply(myCorpus[length(myCorpus)], meta)
#URLs <- lapply(myCorpus, function(x){c(meta(x)["id"], meta(x)["x.uris"])})
URLs <- sapply(myCorpus, function(x){ meta(x)["x.uris"]})
(URLs <- unlist(URLs))

# resolve URLs, time consuming.
# add them to metadata section
pb <- txtProgressBar(min = 0, max = length(myCorpus), style = 3)
i <- 0

myCorpusCopy <-  tm_map(myCorpus, function(x){
        i <<- i + 1
        url <- as.character(meta(x)["x.uris"])
        if(! (url == "character(0)") & nchar(url) > 0){
                printf(paste0(" ", i, ": ", url, "\n"))
                meta(x)["x.uris.resolved"] <- ifelse(nchar(url) > 0, unshorten_url(url), "")
                setTxtProgressBar(pb, i)
        }
        x
})
myCorpusCopy
sapply(myCorpusCopy[1], meta)

(URLS_unshortened <-  lapply(myCorpusCopy, function(x) {c(content(x), meta(x)[["x.uris.resolved"]])}))
#URLS_unshortened <- unique(grep("youtube",URLS_unshortened, value = TRUE, invert = TRUE))


# open urls in browser
res <- sapply(URLS_unshortened, function(x){
#        printf(paste0("# ", x[[1]], "\n"))
        printf(paste0("firefox ", x[[2]], "\n"))  
})
