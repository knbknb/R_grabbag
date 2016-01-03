#
# find all unique users that tweeted about it, get followers
#
# extract user info who tweeted about this
library(twitteR)
#library(tm) 
library(RSQLite)
library(dplyr) # for bind_rows
library(RCurl) # for uri.exists() and resolving urls
setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter/")
source("twitterUtils.R")
source("tmUtils.R")

api_key   =        Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")
access_token            = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret     = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#1
db.name <- paste0("tweets_allkindsof.sqlite")
query.name <- "qry_rstats"
conn <- dbConnect(SQLite(), dbname = db.name)

#try to find a table according to our naming convention
#filter twice 
username.table <- grep("user", dbListTables(conn), ignore.case = TRUE, perl=TRUE, value=TRUE)
username.table <- grep(query.name, username.table, ignore.case = TRUE, perl=TRUE, value=TRUE)
if(length(username.table) <= 0){
        username.table <- paste0(query.name, "_userinfo")
} 

userinfo <- data.frame()
users <- data.frame()
# some magic:
# try to open an EXISTING userinfo table corresponding to the query.name
tryCatch({
        old_users <- dbReadTable(conn, username.table)
        old_users$created <- as.character(as.POSIXct(old_users$created,origin = "1970-01-01"))
        userinfo <- old_users
} ,error=function(e){warning(e); return("cannot open table:")
})


# open table of tweets, containing some NEW users
#if(nrow(userinfo) == 0){
tryCatch({
        old_tweets <- dbReadTable(conn, query.name)
        old_tweets$created <- as.character(as.POSIXct(old_tweets$created,origin = "1970-01-01"))
        users <- unique(old_tweets$screenName)
} ,error=function(e){warning(e); return("cannot open table:")
})

#}
dbDisconnect(conn)
stepsize <- 180 # 180 = twitter rate limit

ratelimits()
#userinfo <-  old_users
# 1:180
pb <- txtProgressBar(min = 0, max = length(users), style = 3)

#set to small value for testing
stepsize2 <- 180 #stepsize
#stepsize2 <- stepsize
for(x in seq(0, length(users), by=stepsize)) {
        for(i in seq(1 + x, x + stepsize2, by=1) ) {
                if(i <= length(users)){
                        tryCatch({
                                tuser <- getUser(users[[i]])
                                userinfo.part <- tuser$toDataFrame()
                                userinfo <<- rbind(userinfo, userinfo.part)
                                }, error=function(e){warning(e); return("cannot get userdata from twitter API:")
                                ppy(userinfo.part)
                        })
                        
                }
                setTxtProgressBar(pb, i)
                Sys.sleep((15 * 60)/stepsize)
        }
}
ratelimits()
append2SQLite(dfr = userinfo, table.name = username.table, db.name = db.name)

stop("finished. next part of script is time-comsuming")

#
#
#
#
#
#
stepsize2 <- 180 #
# resolve anonymous t.co links - THIS IS VERY SLOW
pb <- txtProgressBar(min = 0, max = nrow(userinfo), style = 3)
j <- 0
for(x in seq(0, nrow(userinfo), by=stepsize)) {
        
        for(i in seq(1 + x, x + stepsize2, by=1)) {
                #i =3
                url <- userinfo[i, "url"]
                if(url.exists(url) & (i <= length(userinfo))){
                        userinfo[i, "url"] <- unshorten_url(url)
                        url
                }
                # update progress bar
                j <<- j + 1
                setTxtProgressBar(pb, j)
        }
}


#tweets.langs <-unique( userinfo[grepl("^.", userinfo$lang, perl = TRUE) & !is.na(userinfo$url) ,"lang"])

#tweets.de <- userinfo[grepl("^de", userinfo$lang, perl = TRUE) & !is.na(userinfo$url) ,]
#tweets.de[, "twurl"] <- paste0("google-chrome https://twitter.com/", tweets.de$screenName)
#tail(tweets.de[order(tweets.de$followersCount), c("twurl", "url")], 15)
# open firefox
for(x in 1:nrow(userinfo)) {
        if(! is.na(userinfo[x, "url"])){
                url <- userinfo[x, "url"]
                twurl <- paste0("https://twitter.com/", userinfo[x, "screenName"])
                #printf(paste0("firefox ", url, " ", twurl, "\n"))
                printf(paste0("google-chrome ", url, " ", twurl, " & \n"))
        }
}

#append2SQLite(dfr = userinfo, table.name = username.table, db.name = db.name)

