# fetch tweets data from twitter search API, 
# extract URLs, resolve shortened urls to real urls, 
# open each in browser
library(twitteR)
library(lubridate) # date arrithmetics
library(tm) # used in second half
#library(filehashSQLite) # simple key-value database, for creating a physical corpus
#suppressMessages(library(filehash))
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
days_back <- 9
(date_back <- format(now() - days(days_back), "%Y-%m-%d"))
days_until <- 4
(date_until <- format(now() - days(days_until), "%Y-%m-%d"))
(query.job <- paste0("#potsdam (#job OR #jobs OR #stellenangebot) -RT since:" , date_back, " until:",date_until))

#query.name <- "berlinjobs"1
query.name <- "#32c3"
(query.job <- paste0(query.name, " -RT since:" , date_back, " until:",date_until))
query.name.table <- paste0("qry_32c3")
#
#query <- paste0("#Potsdam -RT since:" , format(now() - days(days_back), "%Y%m%d"))
tweets <- searchTwitter(query.job,n=2000)

# store inside a database, 
#db.name <- "tweets_jobsearch"
db.name <- "tweets_allkindsof"
db.name <- paste0(db.name, ".sqlite")
register_sqlite_backend(db.name)
store_tweets_db(tweets,table_name = query.name.table)
makeTweetsTableUnique(db.name = db.name, table.name=query.name.table)
#store_users_db(tweets,table_name = paste0(query.name, "_users"))
tweets.from_db = load_tweets_db(as.data.frame = TRUE, table_name = query.name.table)

tweets.from_db$longitude <- as.double(tweets.from_db$longitude)
tweets.from_db$latitude <- as.double(tweets.from_db$latitude)
#tweets.df <- twitteR::twListToDF(tweets)

tweets.df <- tweets.from_db[tweets.from_db$longitude > 0 | is.na(tweets.from_db$longitude),]
paste("unique tweets:")
(dim(tweets.df))
#print as YAML
ppy(tail(tweets.df, 2))

# top usernames posting (about) jobs
scrnames <- tapply(tweets.df$text, tweets.df$screenName, length)
(scrnames[names(scrnames[order(scrnames, decreasing = TRUE)])[1:10]])

# filter results
searchstr <- "dat|soft|analy|geo|min|syst|bio"
tweets.df <- tweets.df[grepl(searchstr, tweets.df$text, perl=TRUE, ignore.case = TRUE),]
negstr <- " NY|Oracle|Berlin,?\\s*WI|Berlin,?\\s*WI|"
tweets.df <- tweets.df[!grepl(negstr, tweets.df$text, perl=TRUE, ignore.case = FALSE),]
paste(" tweets about: ", searchstr)
(dim(tweets.df))
#print a few as YAML
ppy(tail(tweets.df[,"text"], nrow(tweets.df)))

#build corpus, simple way
#myCorpus <- Corpus(VectorSource(as.vector(tweets.df.text)))

# build corpus, from fweet-dataframe
dfsrc <- DataframeSource(tweets.df)
myCorpus <- Corpus(dfsrc,
                    readerControl = list(reader=commonReader()))


myCorpus <- tm_map(myCorpus, content_transformer(tm_convertToUTF8))

myStopwords <- c("<ed><ae><ba><ed><be><85", "<ed><ae><ba><ed><be><85>")
myCorpus <- tm_map(myCorpus, content_transformer(tm_removeStopwords), myStopwords)

# first 3 words become 'heading' metadata entity
myCorpus <- tm_map(myCorpus, setHeading)

#myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus <- tm_map(myCorpus, content_transformer(tm_removeNonAlnum))

# would invalidate case-sensitive urls by twitter (t.co)
#myCorpus <- tm_map(myCorpus, content_transformer(tolower)) 
tm_shown_meta(myCorpus, ndoc=2)

#

# 'id' metadata entity becomes 'description' plus 'screenName' - easier for DTMs
tryCatch({myCorpus <- tm_map(myCorpus, setId)}, error=function(e){warning(e); return("cannot set heading:")})
# add a humanreadable datestring
myCorpus <- tm_map(myCorpus, setDatestr)


myCorpus
sapply(myCorpus[1], meta)
res <- sapply(myCorpus[randn], function(x){ppy(content(x))})

myStopwords <- c(stopwords("english"), stopwords("german"), "rt", "@", "-", "via")
myCorpus <- tm_map(myCorpus, content_transformer(tm_removeStopwords), myStopwords)
tm_shown_meta(myCorpus, ndoc=2)


# # only retweeted multiple times
# myCorpus <- tm_filter(myCorpus, function(x){
#         meta(x, tag="retweetCount") > 0
# })

# most urls are anonymous t.co urls, created by twitter, example:
extractURL(content(myCorpus[[1]]))

# put original urls into metadata section of the tm corpus
myCorpus <- tm_map(myCorpus, function(x){meta(x)["x.uris"] <- extractURL(content(x)); x})
tm_shown_meta(myCorpus, ndoc=2)

#URLs <- lapply(myCorpus, function(x){c(meta(x)["id"], meta(x)["x.uris"])})

URLs <-tm_shown_meta(myCorpus, ndoc=10,tag="x.uris")
(URLs <- unlist(URLs))

# resolve URLs, time consuming.
# add them to metadata section
#length(myCorpus)
pb <- txtProgressBar(min = 0, max = 3, style = 3)
i <- 0
timestamp()
myCorpusCopy <-  tm_map(myCorpus[10:12], function(x){
        i <<- i + 1
        url <- as.character(meta(x)["x.uris"])
        if(! (url == "character(0)") & nchar(url) > 0){
                printf(paste0(" ", i, ": ", url, "\n"))
                meta(x)["x.uris.resolved"] <- ifelse(nchar(url) > 0, unshorten_url(url), "")
                setTxtProgressBar(pb, i)
        } else { printf("cannot unshorten url: ", url)}
        x
})
timestamp()

library(openNLP)
help(openNLP)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(content(myCorpus[[1]]), list(sent_token_annotator, word_token_annotator))



tm_shown_meta(myCorpusCopy, ndoc=3)

(URLS_unshortened <-  lapply(myCorpusCopy, function(x) {c(content(x), meta(x)[["x.uris.resolved"]])}))
#URLS_unshortened <- unique(grep("youtube",URLS_unshortened, value = TRUE, invert = TRUE))


# open urls in browser
res <- sapply(URLS_unshortened, function(x){
#        printf(paste0("# ", x[[1]], "\n"))
        printf(paste0("firefox ", x[[2]], "\n"))  
})
