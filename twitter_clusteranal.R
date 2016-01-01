# fetch twitter data, clean tweets, remove URLs
# create wordcloud,
# perform cluster analysis,
# extract user info who tweeted about this
library(twitteR)
library(lubridate) # days back
library(tm) 
library(Hmisc) # for %nin%
library("wordcloud")
library(dplyr) # for bind_rows
setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter/")
source("twitterUtils.R")

##Authenticate here to query the twitter API##
###You need to create an app at https://apps.twitter.com/app/ ###
#sysenv_search("TW_APP")
#sysenv_search("TW_APP_RCMDR")
api_key   =        Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")
access_token            = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret     = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")

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
days_back <- 10
date_back <- format(now() - days(days_back), "%Y%m%d")
#query <- paste0("#potsdam -#nowplaying -RT since:" , date_back)
query <- paste0("#tatort -RT since:" , date_back)
query.name <- "qry_tatort"
tweets <- searchTwitter(query,n=1000)
# store inside a database, 
db.name <- paste0("tweets_allkindsof.sqlite")
register_sqlite_backend(db.name)

#store one tweet to check if sqlite is available
is_stored <- store_tweets_db(tweets[1],table_name = query.name)
if (is_stored == TRUE){
        #load all from db, merge with new, remove duplicates, store
        tweets.df <- twitteR::twListToDF(tweets)
        tweets.old <- load_tweets_db(as.data.frame = FALSE, table_name = query.name)
        #tweets.df = rbind(tweets.df, twitteR::twListToDF(tweets.old))
        # truncate the table 
        con <- dbConnect(SQLite(), dbname = db.name)
        rs <- dbSendQuery(con, paste0("delete from ", query.name))
        dbDisconnect(con)
        # query
        
        is_stored2 <- store_tweets_db(unique(c(tweets, tweets.old)),table_name = query.name)
        #store_users_db(tweets,table_name = paste0(query.name, "_users"))
        tweets.df = unique(load_tweets_db(as.data.frame = TRUE, table_name = query.name))
        #tweets.df <- unique(tweets.df[tweets.df$longitude > 0 | is.na(tweets.df$longitude), ])
} else {
        tweets.df <- twitteR::twListToDF(tweets)
}
tweets.df <- unique(tweets.df)

###  create the corpus from a Dataframe of tweets
## simple way:
#myCorpus <- Corpus(VectorSource(as.vector(tweets.df[tweets.df$text,])))

## complex but more inforation-rich way: WITH document metadata
myCorpus <- DataframeSource(tweets.df)
myCorpus <- Corpus(myCorpus,
                    readerControl = list(reader=commonReader()))

shown <- 15
shown <- min(shown, length(myCorpus))
randn <- sample(x=length(myCorpus), size=shown)
#myCorpus <- tm_map(myCorpus, content_transformer(tm_convertToUTF8))
#sapply(myCorpus[which(!is.na(sapply(myCorpus, content)))], content)
tryCatch({sapply(randn, function(i) {content(myCorpus[[i]])})}, error=function(e){warning(e); return("cannot show content:")})
sapply(randn, function(i) {meta(myCorpus[[i]], tag="author")})
sapply(randn, function(i) {meta(myCorpus[[i]], tag="retweetCount")})
sapply(randn, function(i) {meta(myCorpus[[i]])})


# process tweets: remove weird characters, URLs, most hashtags; convert to lowercase 
myCorpus.URLs.removed <- tm_map(myCorpus, content_transformer(tm_convertToUTF8))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(removeURL))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(tm_removeNonAlnum))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(tolower))

# first 3 words become 'heading' metadata entity
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, setHeading)

# 'id' metadata entity becomes 'description' plus 'screenName' - easier for DTMs
tryCatch({myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, setId)}, error=function(e){warning(e); return("cannot set heading:")})



myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, setDatestr)



# again, show a few
sapply(randn, function(i) {meta(myCorpus.URLs.removed[[i]])})
tryCatch({sapply(randn, function(i) {content(myCorpus.URLs.removed[[i]])})}, error=function(e){warning(e); return("cannot show content:")})


sapply(randn, function(i) {tm::termFreq(myCorpus.URLs.removed[[i]])})



myStopwords <- c(stopwords("english"), stopwords("german"), "rt", "@", "-", "via")

# Create a wordcloud, Doc-term Matrix, cluster-analyis, Plot
myCorpusCopy <- tm_map(myCorpus.URLs.removed, content_transformer(tm_removeStopwords), myStopwords)
sapply(randn, function(i) {meta(myCorpusCopy[[i]])})
myCorpusCopy <- tm_filter(myCorpusCopy, function(x){
        meta(x, tag="retweetCount") > 0
})

wordcloud(myCorpusCopy, min.freq=5)

tdm <- TermDocumentMatrix(myCorpusCopy,control=list(wordLengths=c(1,Inf)))
tdm <- tdm[Terms(tdm) %nin% myStopwords,]

tm_freqterms(tdm, topn = 115)
        
tdm2 <- removeSparseTerms(tdm, sparse = 0.97)
#str(tdm2)
tm_freqterms(tdm2, topn = 115)


matrix2 <- as.data.frame(as.matrix(tdm2))
dim(matrix2)
head(t(matrix2))

# cluster terms
distMatrix <- dist(scale(matrix2))
fit <- hclust(distMatrix, method = "ward.D2")
old.par <- par(mar = c(0, 0, 0, 0))
par(mar = par("mar") + c(0,0,0,50))

#outfile <- paste0(query.name, "--", date_back, "-dendrogram.svg")
outfile <- paste0(query.name, "--test-", date_back, "-dendrogram.png")

dopng(outfile, cmd=
  plot(as.dendrogram(fit), horiz = TRUE, frame.plot = FALSE,
     xlab="", ylab="", yaxt='n', 
     main = paste0("clustered Tweets for \n'", query, "'"))
)
system(paste0("firefox ", outfile))
