#
# find all unique users that tweeted about it, get followers
#
# extract user info who tweeted about this
library(twitteR)
#library(tm) 
library(RSQLite)
library(dplyr) # for bind_rows
library(RCurl) # for uri.exists() and resolving urls
setwd("/home/knut/code/git/_my/tweets2sqlite")
source("twitterUtils.R")
source("tmUtils.R")

api_key   =        Sys.getenv("TW_APP_RCMDR_APIKEY")
api_secret=        Sys.getenv("TW_APP_RCMDR_APISEC")
access_token            = Sys.getenv("TW_APP_RCMDR_ACCTOK")
access_token_secret     = Sys.getenv("TW_APP_RCMDR_ACCTOKSEC")
options(httr_oauth_cache=TRUE)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#1
db.name <- paste0("tweets_allkindsof.sqlite")
#db.name <- paste0("tweets_jobsearch.sqlite")

query.name <- "qry_code2015"
#query.name <- "hamburgjobs_status"
conn <- dbConnect(SQLite(), dbname = db.name)

table.type = "users"
#table.type = "followers"

#try to find a table according to our naming convention
#filter twice 
if (table.type == "followers"){
        table.postfix <- "_followerinfo"        
        table.searchstr <- "follower"        
        table.users_followers <- paste0(query.name, "_users_followers")
        try({
                sql <- paste0('CREATE TABLE "', table.users_followers,'" ("userid" INTEGER NOT NULL , "followerid" INTEGER NOT NULL )')
        sth <- dbSendQuery(conn, sql)
        rm
        }, silent = TRUE)
        
} else {
        table.postfix <- "_userinfo"
        table.searchstr <- "userinfo"
}

username.table <- grep(table.searchstr, dbListTables(conn), ignore.case = TRUE, perl=TRUE, value=TRUE)
username.table <- grep(query.name, username.table, ignore.case = TRUE, perl=TRUE, value=TRUE)
if(length(username.table) <= 0){
        username.table <- paste0(query.name, table.postfix)
} 
# sql = paste0("SELECT distinct name from ", username.table)  
# res <- dbSendQuery(conn, sql)
# users.known <- dbFetch(res)
# dbClearResult(res)
userinfo <- data.frame()
users <- data.frame()
users_followers <- data.frame()
# some magic:
# try to open an EXISTING userinfo table corresponding to the query.name
tryCatch({
        old_users <- dbReadTable(conn, username.table)
        #old_users$created <- as.character(as.POSIXct(old_users$created,origin = "1970-01-01"))
        userinfo <- old_users
} ,error=function(e){warning(e); return("cannot open table:")
})


# open table of tweets, containing some NEW users
#if(nrow(userinfo) == 0){
tryCatch({
        old_tweets <- dbReadTable(conn, query.name)
        #old_tweets$created <- as.character(as.POSIXct(old_tweets$created,origin = "1970-01-01"))
        users <- unique(old_tweets$screenName)
} ,error=function(e){warning(e); return("cannot open table:")
})

# only lookup new users, only if table exists
if(exists("old_users")){
        (users <- sort(setdiff(old_tweets$screenName, old_users$screenName)))
}
dbDisconnect(conn)
ratelimit_userget <- 180 # 180 = twitter rate limit 

ratelimits()

pb <- txtProgressBar(min = 0, max = length(users), style = 3)

batchsize_userget <- 100 # for fetching users from twitter

users_cnt <- length(users)
#blockonratelimit


printf(paste0("fetching info for ", length(users), " users\n"))
#for(x in seq_along(1:(users_cnt %% ratelimit_userget))) {
        timestamp()
        #for(i in seq(1 + x, x + batchsize_userget, by=batchsize_userget )) {
        x <- 1
        for(k in seq(x, length(users), by=batchsize_userget )){
                i <- k + 1
                #k <- i + batchsize_userget
                if(i <= length(users)){
                        tryCatch({
                                #printf(paste0(i,"/",length(users),  ": " , users[[i]], "  "))
                                if (table.type == "followers"){
                                        printf(paste0("\nGetting followers...  "))
                                        followees <- lookupUsers(users[i:i+ min(batchsize_userget, length(users))])
                                        #user$getFollowerIDs(n=1)
                                        
                                        tusers <- lapply(followees, function(user) {
                                                getFollowersList(followees[1], nMax = 1000)
                                        })
                                        # all follwers to single data frame
                                        res <- lapply(tusers, function(tuser){
                                                userinfo.part <- tuser$toDataFrame()
                                                userinfo <<- rbind(userinfo, userinfo.part)
                                                # add an entry to bridging table
                                                users_followers.part <- data.frame(userid=user$id, followerid=tuser$id)
                                                append2SQLite(dfr = users_followers.part, table.name = table.users_followers, db.name = db.name)
                                        })
                                } else {
                                        printf(paste0("\nRun ", x,  ": Getting users...  i >= ", i , ""))
                                        #tuser <- getUser(users[[i]])
                                        tusers <- lookupUsers(users[i:min(i+batchsize_userget, length(users))])
                                        res <- lapply(tusers, function(tuser){
                                                userinfo.part <- tuser$toDataFrame()
                                                userinfo <<- rbind(userinfo, userinfo.part)
                                                # add an entry to bridging table
                                        })
                                        #userinfo.part <- tuser$toDataFrame()
                                        #userinfo <<- rbind(userinfo, userinfo.part)
                                }
                                
                                }, error=function(e){warning(e); return("cannot get userdata from twitter API:")
                                ppy(userinfo.part)
                        })
                        
                }
                setTxtProgressBar(pb, i)
                Sys.sleep((15 * 60)/ratelimit_userget)
        }
#}
timestamp()
ratelimits()
#userinfo.dates.epoch <- userinfo[grepl("^\\d+$",userinfo$created, perl=TRUE),]
#userinfo.dates.epoch[, "created"] <- as.character(as.POSIXct(as.integer(userinfo.dates.epoch[, "created"]),origin = "1970-01-01"))

append2SQLite(dfr = unique(userinfo), table.name = username.table, db.name = db.name)
 

makeUserTableUnique(db.name = db.name, table.name=username.table)


stop("finished. next part of script is time-comsuming")

#
#
#
#
#
#
batchsize_userget <- 180 #
# resolve anonymous t.co links - THIS IS VERY SLOW
pb <- txtProgressBar(min = 0, max = nrow(userinfo), style = 3)
j <- 0
for(x in seq(0, nrow(userinfo), by=ratelimit_userget)) {
        timestamp()
        for(i in seq(x, length(users), by=batchsize_userget)) {
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
timestamp()

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

