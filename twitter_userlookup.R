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
db.name <- paste0("db/tweets_allkindsof.sqlite")
#db.name <- paste0("tweets_jobsearch.sqlite")

query.name <- "qry_agrardemo"
conn <- dbConnect(SQLite(), dbname = db.name)

table.type = "users"
#table.type = "followers"

ratelimit_userget <- 180 # 180 = twitter rate limit 

#try to find a table according to our naming convention
#filter twice 
if (table.type == "followers"){
        table.postfix <- "_followerinfo"        
        table.searchstr <- "followerinfo"        
        table.users_followers <- paste0(query.name, "_users_followers")
        # bridging table
        try({
                sql <- paste0('CREATE TABLE "', table.users_followers,'" ("userid" INTEGER NOT NULL , "followerid" INTEGER NOT NULL )')
        sth <- dbSendQuery(conn, sql)
        rm
        }, silent = TRUE)
        
        batchsize_userget <- 1 # for looking up follower-lists from twitter
        
} else {
        table.postfix <- "_userinfo"
        table.searchstr <- "userinfo"
        
        batchsize_userget <- 180 # for fetching users from twitter
        
}


username.tables <- grep(table.searchstr, dbListTables(conn), ignore.case = TRUE, perl=TRUE, value=TRUE)
username.table <- grep(paste0(query.name, table.postfix, "$"), username.tables, ignore.case = TRUE, perl=TRUE, value=TRUE)
if(length(username.table) <= 0){
        username.table <- paste0(query.name, table.postfix)
} 


userinfo <- data.frame()
users <- data.frame()
users_followers <- data.frame()
users_followers <- data.frame()
# some magic:
# try to open an EXISTING userinfo table corresponding to the query.name
tryCatch({
        old_users <- dbReadTable(conn, paste0(query.name, "_userinfo"))
        #old_users$created <- as.character(as.POSIXct(old_users$created,origin = "1970-01-01"))
        userinfo <- old_users
} ,error=function(e){warning(e); 
        return("cannot open table:")
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
if(exists("old_users") & table.type == "users"){
        (users <- sort(setdiff(old_tweets$screenName, old_users$screenName)))
} else if(exists("old_users") & table.type == "followers"){
        user.without.followerinfo <- dbReadTable(conn, table.users_followers)
        users <- sort(setdiff(old_users$screenName,  unique(user.without.followerinfo$userScreenName)))
        rev(users)
}
dbDisconnect(conn)

ratelimits()

pb <- txtProgressBar(min = 0, max = length(users), style = 3)

users_cnt <- length(users)
#retryOnRateLimit



printf(paste0("fetching info for ", length(users), " users\n"))
#for(x in seq_along(1:(users_cnt %% ratelimit_userget))) {
timestamp()

x <- 1

#for(k in seq(x, length(users), by=batchsize_userget )){
for(k in seq(1, length(users), by=batchsize_userget )){
        i <- k 
        #k <- i + batchsize_userget
        if(i <= length(users)){
                tryCatch({
                        printf(paste0(" ", i,"/",length(users),  ": " , users[[i]], "  "))
                        rlim <-  getCurRateLimitInfo()
                        
                        if (table.type == "followers" & as.integer(rlim[rlim$resource == "/followers/ids", "remaining"]) > 0){
                                printf(paste0("\nGetting followers...  "))
                                #, retryOnRateLimit=9000
                                followees <- lookupUsers(users[i:min(batchsize_userget, length(users))])
                                #followees <- lookupUsers(users[1:10])
                                #user$getFollowerIDs(n=1)
                                printf(sapply(followees, "[[", "screenName"))
                                tusers <- lapply(as.list(followees), function(user) {
                                        list(followers = getFollowersList(user, nMax = 1000, retryOnRateLimit=10000),
                                             username = user$getScreenName())
                                })
                                #tusers[[1]][["followers"]]
                                # all follwers to single data frame

                                res <- lapply(tusers, function(followee){
                                        #screenName <- min(gsub("^(\\S+)(\\.).+(\\..+$)", "\\1", names(tuser), perl=TRUE))
                                        #screenName <- followee$getScreenName()
                                        screenName <- followee[["username"]]
                                        printf(paste0(screenName, "\n"))
                                        printf(paste0("\nlooking up followers: ",screenName))
                                        if(is.character(screenName)){
                                                printf(paste0("\n"))
                                                lapply(followee[["followers"]], function(follower){
                                                        userinfo.part <- twitteR::twListToDF(follower)
                                                        userinfo <<- rbind(userinfo, userinfo.part)
                                                        # add an entry to bridging table
                                                        users_followers.part <- data.frame(userScreenName=rep(screenName, nrow(userinfo.part)), followerScreenName=userinfo.part$screenName)
                                                        users_followers <<- rbind(users_followers, userinfo.part)
                                                        #append2SQLite(dfr = users_followers.part, table.name = table.users_followers, db.name = db.name)
                                                })
                                        } else{
                                                printf(paste0("can't get followers for ", followee$getScreenName(), ": ratelimit exceeeded? \n"))
                                                
                                        }
                                })
                        } else {
                                printf(paste0("\nRun ", x,  ": Getting users...  i >= ", i , ""))
                                #tuser <- getUser(users[[i]])
                                tusers <- lookupUsers(users[i:min(i+batchsize_userget, length(users))])
                                res <- lapply(tusers, function(tuser){
                                        userinfo.part <- tuser$toDataFrame()
                                        userinfo <<- rbind(userinfo, userinfo.part)
                                        
                                })
                                #userinfo.part <- tuser$toDataFrame()
                                #userinfo <<- rbind(userinfo, userinfo.part)
                        }
                        
                        }, error=function(e){warning(e); return("cannot get userdata from twitter API:")
                        #ppy(userinfo.part)
                })
                
        }
        setTxtProgressBar(pb, i)
        Sys.sleep((15 * 60)/ratelimit_userget)
}

        
timestamp()
ratelimits()
#userinfo.dates.epoch <- userinfo[grepl("^\\d+$",userinfo$created, perl=TRUE),]
#userinfo.dates.epoch[, "created"] <- as.character(as.POSIXct(as.integer(userinfo.dates.epoch[, "created"]),origin = "1970-01-01"))
userinfo[, "row_names"] <- NULL

append2SQLite(dfr = users_followers, table.name = table.users_followers, db.name = db.name)

append2SQLite(dfr = unique(userinfo), table.name = username.table, db.name = db.name)
#append2SQLite(dfr = unique(userinfo[,1:16]), table.name = username.table, db.name = db.name)

createView4DuplicateIDs(table.name = username.table, db.name = db.name)



makeUserTableUnique(db.name = db.name, table.name=username.table)
#rm(user.tablename.augmented)
username.table.augmented <- paste0(username.table, "_augmented")
userinfo.augmented <- data.frame(id = userinfo[, "id"], 
                                 created = as.character(as.POSIXct(as.integer(userinfo[, "created"]),origin = "1970-01-01")),
                                 followersPerWeek = userinfo$followersCount/
                                         (age(dob=as.POSIXct(as.integer(userinfo[, "created"]),origin = "1970-01-01"), units = "weeks")+1),
                                tweetsPerWeek = userinfo$statusesCount/
                                        (age(dob=as.POSIXct(as.integer(userinfo[, "created"]),origin = "1970-01-01"), units = "weeks")+1))
#plot(userinfo.augmented$tweetsPerWeek, log10(userinfo.augmented$followersPerWeek), ylim=c(0,1))
addAugmentedTable(db.name = db.name, table.name=username.table.augmented, table.main.name = username.table)
append2SQLite(dfr = unique(userinfo.augmented), table.name = username.table.augmented, db.name = db.name)                  
makeTableUnique(db.name = db.name, table.name=username.table.augmented)
#dbDisconnect(conn)
stop("finished. next part of script is t.co url-resolution, very time-consuming")

#
#
#
#
#
#
batchsize_userget <- 15 #
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

