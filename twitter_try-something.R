library(twitteR)

#setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter")
# my App: Rcmdr-tm
sysenv_get("TW")
api_key=Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")

access_token = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1

tuser <- getUser('sudo_f')

tuser2 = friendships("sudo_f")
tuser$getFollowerIDs()

woeids <- availableTrendLocations()
unique(getTrends(woeids[woeids$country =='Germany',]$woeid)$name)
#sapply(        , FUN=function(x){x})
