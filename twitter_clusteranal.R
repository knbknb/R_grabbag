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

(query <- paste0("#MSC2016 OR @munsecconf  -RT since:" , date_back, " until:",date_until))
query.name <- "qry_munsecconf"
table.name <- query.name

# if false, read tweets from database 
# if true,  perform new query
online_query = FALSE
if(online_query){
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
} else {
        db.name <- paste0("db/tweets_allkindsof.sqlite")
        #db.name <- paste0("tweets_jobsearch.sqlite")
        
        query.name <- "qry_munsecconf"
        conn <- dbConnect(SQLite(), dbname = db.name)
        
        db.name <- paste0("db/tweets_allkindsof.sqlite")
        register_sqlite_backend(db.name)
        table.name <- query.name
        
        tweets.df = load_tweets_db(as.data.frame = TRUE, table_name = table.name)
        
}
###  create the corpus from a Dataframe of tweets
## simple way:
#myCorpus <- Corpus(VectorSource(as.vector(tweets.df[tweets.df$text,])))

## complex but more inforation-rich way: WITH document metadata
myCorpus <- DataframeSource(tweets.df)
myCorpus <- Corpus(myCorpus,
                    readerControl = list(reader=commonReader()))

tm_shown_content(corpus = myCorpus, ndoc = 5)

# we want to create plots: 
# process tweets: remove weird characters, URLs, most hashtags; convert to lowercase 
myCorpus.URLs.removed <- tm_map(myCorpus, content_transformer(tm_convertToUTF8))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(removeURL))
#myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(tm_removeNonAlnum))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(removeFirstChars))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(tolower))

# first 3 words become 'heading' metadata entity
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, setHeading)
# 'id' metadata entity becomes 'description' plus 'screenName' - easier for DTMs
tryCatch({myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, setId)}, error=function(e){warning(e); return("cannot set heading:")})
# add a humanreadable datestring
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, setDatestr)


# again, show a few
tm_shown_meta(corpus = myCorpus.URLs.removed, ndoc=5)
tm_shown_meta(corpus = myCorpus.URLs.removed, ndoc=5, tag="author")
tm_shown_meta(corpus = myCorpus.URLs.removed, ndoc=2, tag="retweetCount")
tm_shown_content(corpus = myCorpus.URLs.removed, ndoc=2)


myStopwords <- c(stopwords("english"), stopwords("german"), "rt", "&amp;","!", ",", "@", "-", "via")
myCorpusCopy <- tm_map(myCorpus.URLs.removed, content_transformer(tm_removeStopwords), myStopwords)

myCorpusCopy <- tm_filter(myCorpusCopy, function(x){
        meta(x, tag="retweetCount") > 0
})

# Create a wordcloud, Doc-term Matrix, cluster-analyis, Plot
wordcloud(myCorpusCopy, min.freq=5)

tdm <- TermDocumentMatrix(myCorpusCopy,control=list(wordLengths=c(1,Inf)))
tdm <- tdm[setdiff(Terms(tdm),myStopwords),]

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
(outfile <- paste0(query.name, "--test-", date_back, "-dendrogram.png"))

dopng(outfile, cmd=
  plot(as.dendrogram(fit), horiz = TRUE, frame.plot = FALSE,
     xlab="", ylab="", yaxt='n', 
     main = paste0("clustered Tweets for \n'", query, "'"))
)
printf(paste0("firefox ", outfile))
