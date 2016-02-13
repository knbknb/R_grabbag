# load tweets from db, determine language of tweet with NLP methods,
# write back to database
library(tm)
library(textcat)
library(dplyr)
library(twitteR) # for sqlite stuff
library(rJava) # for coreNLP
library(coreNLP) # for sentiment Analysis

source("tmUtils.R")
source("twitterUtils.R")
db.name <- paste0("db/tweets_allkindsof.sqlite")
register_sqlite_backend(db.name)
table.name <- "qry_munsecconf"

tweets.df = load_tweets_db(as.data.frame = TRUE, table_name = table.name)

## complex but more inforation-rich way: WITH document metadata
myCorpus <- DataframeSource(tweets.df)
myCorpus <- Corpus(myCorpus, readerControl = list(reader=commonReader()))

tm_shown_content(corpus = myCorpus, ndoc = 5)

# we want to create plots: 
# process tweets: remove weird characters, URLs, most hashtags; convert to lowercase 
myCorpus.URLs.removed <- tm_map(myCorpus, content_transformer(tm_convertToUTF8))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(removeURL))
myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(removeFirstChars))
#myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(tm_removeNonAlnum))
#myCorpus.URLs.removed <- tm_map(myCorpus.URLs.removed, content_transformer(tolower))

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


myStopwords <- c(stopwords("english"), stopwords("german"), "rt", "@", "-", "via")
#myCorpusCopy <- tm_map(myCorpus.URLs.removed, content_transformer(tm_removeStopwords), myStopwords)
myCorpusCopy <- myCorpus.URLs.removed
#sapply(myCorpusCopy, function(x){       print(c(content(x),meta(x, tag="language"))) })

#sapply(myCorpusCopy, function(x){       print(c(content(x),meta(x))) })

# my.profiles <- TC_byte_profiles[names(TC_byte_profiles) %nin% c("afrikaans",
#                                                                 "basque",
#                                                                 "frisian",
#                                                                 "latin",
#                                                                 "polish",
#                                                                 "spanish",
#                                                                 "welsh")]
my.profiles <- TC_char_profiles[names(TC_char_profiles) %nin% c("afrikaans",
                                                                "basque",
                                                                "frisian","middle_frisian",
                                                                "latin",
                                                                "rumantsch",
                                                                "spanish",
                                                                "welsh",
                                                                "catalan",
                                                                "hungarian",
                                                                "romanian",
                                                                "scots",
                                                                "swedish")]
my.profiles <- TC_char_profiles[names(TC_char_profiles) %nin% c("german",
                                                                "english")]
my.profiles <- ECIMCI_profiles[names(ECIMCI_profiles) %nin% c("afrikaans",
                                       "basque",
                                       "frisian","middle_frisian",
                                       "latin",
                                       "rumantsch",
                                       "spanish",
                                       "welsh",
                                       "catalan",
                                       "hungarian",
                                       "romanian",
                                       "scots",
                                       "swedish")]

initCoreNLP()

myCorpusCopy <- tm_map(myCorpusCopy, function(x){
        #lang <- textcat::textcat(content(x))
        lang <- textcat::textcat(content(x), p=my.profiles)
        #warning(lang)
        meta(x, tag="language") <- lang
        x
})

sIn <- "Mother died today. Or, maybe, yesterday; I can't be sure."
annoObj <- annotateString(content(myCorpusCopy[[4]]))
getSentiment(annoObj)
# 

myCorpusCopy <- tm_map(myCorpusCopy, content_transformer(tm_removeNonAlnum))

myCorpusCopy2 <- tm_filter(myCorpusCopy, function(x){
        any(class(x) != "NULL")
})
myCorpusCopy2 <- tm_map(myCorpusCopy2, function(x){
        try({
                meta(x, tag="sentiment") <- getSentiment(annotateString(content(x)))$sentiment[1]
        })
        x
})


content(myCorpusCopy2)
meta(myCorpusCopy2[[3]])
content(myCorpusCopy2[[9]])
#sapply(myCorpusCopy, function(x){       print(c(content(x),meta(x, tag="language"))) })
tm_shown_meta(corpus = myCorpusCopy2, ndoc=15, tag="language")
tm_shown_meta(corpus = myCorpusCopy2, ndoc=15, tag="sentiment")

tm_shown_content(corpus = myCorpusCopy2, ndoc=20)

tweets.df2 <- do.call(bind_rows, lapply(myCorpusCopy2, function(x){       
        # print(c(content(x),meta(x, tag="language"))) 
        list(id=as.character(meta(x, tag="description")), 
             language=as.character(meta(x, tag="language")),
             datetimestr=as.character(meta(x, tag="datetime")),
             sentiment=as.character(meta(x, tag="sentiment"))
        )
}))

tweets.df2 <- tweets.df %>%
        left_join(by="id", x=., y=tweets.df2)

table.name.augmented <- paste0(table.name, "_augmented")
addAugmentedTweetsTable(db.name = db.name, table.name = table.name.augmented, table.main.name = table.name)

createView4AugmentedTweets(tweets.df2, db.name, table.name.augmented, table.name)
#makeTweetsTableUnique(db.name = db.name, table.name=table.name.augmented)

makeTableUnique(db.name = db.name, table.name=table.name)
makeTableUnique(db.name = db.name, table.name=table.name.augmented)
makeTableUnique2(db.name = db.name, table.name=table.name)
