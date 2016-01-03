# fetch tweets data from twitter search API, 
# extract URLs, resolve shortened urls to real urls, 
# open each in browser
library(twitteR)
library(lubridate) # date arrithmetics
library(tm) # used in second half
library(filehashSQLite) # simple key-value database, for creating a physical corpus
#suppressMessages(library(filehashSQLite))
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
days_back <- 1
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
db.name <- "tweets_jobsearch"
register_sqlite_backend(paste0(db.name, ".sqlite"))

store_tweets_db(tweets,table_name = query.name.table)
#store_users_db(tweets,table_name = paste0(query.name, "_users"))
tweets.from_db = load_tweets_db(as.data.frame = TRUE, table_name = query.name.table)
(dim(tweets.from_db))
# only take tweets with geocoordinates
tweets.from_db$longitude <- as.double(tweets.from_db$longitude)
tweets.from_db$latitude <- as.double(tweets.from_db$latitude)
tweets.df <- unique(tweets.from_db[tweets.from_db$longitude > 0 | is.na(tweets.from_db$longitude),])
paste("unique tweets:")
(dim(tweets.df))
#print as YAML
ppy(tail(tweets.df, 2))

# top usernames posting (about) jobs
scrnames <- tapply(tweets.df$text, tweets.df$screenName, length)
(scrnames[names(scrnames[order(scrnames, decreasing = TRUE)])[1:10]])

# filter results
searchstr <- "data|soft"
tweets.df <- tweets.df[grepl(searchstr, tweets.df$text, perl=TRUE, ignore.case = TRUE),]
paste("unique tweets about: ", searchstr)
(dim(tweets.df))
#print as YAML
ppy(tail(tweets.df, 2))

#build corpus, simple way
#myCorpus <- Corpus(VectorSource(as.vector(tweets.df.text)))

# build corpus, from fweet-dataframe
dfsrc <- DataframeSource(tweets.df)
myCorpus <- Corpus(dfsrc,
                    readerControl = list(reader=commonReader()))

# read in physical corpus , combine with new tweets
s <- paste0(db.name, "_", query.name, "_sqlite") #replace mydat with something more descriptive 
if( file.exists(s)){
        db <- dbInit(s, "SQLite")
        pc <- dbLoad(db)
        myCorpus <- c(pc, myCorpus)
} else {
        #pc = PCorpus(DataframeSource(csv), readerControl = list(language = "en"), dbControl = list(dbName = s, dbType = "SQLite"))
        dfsrc <- DataframeSource(tweets.df)
        myCorpus <- Corpus(dfsrc,
                           readerControl = list(reader=commonReader(),
                                                dbControl = list(dbName = s, dbType = "SQLite")))
        dbCreate(s, "SQLite")
        db <- dbInit(s, "SQLite")
        #set.seed(234)
        # add another record, just to show we can.
        # key="test", value = "Hi there"
        #dbInsert(db, "test", "hi there")
} 
        

shown <- 15
shown <- min(length(myCorpus), shown)
randn <- sample(length(myCorpus), shown)
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


myCorpusCopy



tm_shown_meta(myCorpusCopy, ndoc=3)

(URLS_unshortened <-  lapply(myCorpusCopy, function(x) {c(content(x), meta(x)[["x.uris.resolved"]])}))
#URLS_unshortened <- unique(grep("youtube",URLS_unshortened, value = TRUE, invert = TRUE))


# open urls in browser
res <- sapply(URLS_unshortened, function(x){
#        printf(paste0("# ", x[[1]], "\n"))
        printf(paste0("firefox ", x[[2]], "\n"))  
})
