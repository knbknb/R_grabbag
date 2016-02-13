library(twitteR)

#setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter")
# my App: Rcmdr-tm
sysenv_get("TW")
api_key=Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")

access_token = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


tuser <- getUser('sudo_f')

tuser2 = friendships("sudo_f")
tuser$getFollowerIDs()

woeids <- availableTrendLocations()
unique(getTrends(woeids[woeids$country =='Germany',]$woeid)$name)
#sapply(        , FUN=function(x){x})

library(rJava)
library(coreNLP)
#library(openNLP)
initCoreNLP()
#downloadCoreNLP()
sIn <- "Mother died  today. Or, maybe, yesterday; I can't be sure."
annoObj <- annotateString(sIn)
as.character(getSentiment(annoObj))
getwd()
initCoreNLP("/home/knut/R/x86_64-pc-linux-gnu-library/3.2/coreNLP/extdata/stanford-german-2015-10-14")
