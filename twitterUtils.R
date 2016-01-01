########################################
## Put everything into an environment, to not pollute global namespace

twitterUtil <- new.env()

# this function patches a problem with the lookup function
# ignore for now
twitterUtil$patchLookupUsers <- function(users, ...)
  {
    batches <- split(users, ceiling(seq_along(users)/100))
    results <- lapply(batches, function(batch) {
      params <- twitteR:::parseUsers(batch)
      twitteR:::twInterfaceObj$doAPICall(paste("users", "lookup", sep = "/"), 
                                         params = params, ...)
    })
    out <- sapply(do.call(c, results), twitteR:::buildUser)
    out
  }

# get a user's followers and their location
twitterUtil$getFollowers <- function(user, nMax=1000, ...)
  {
    followers=patchLookupUsers(user$getFollowerIDs(n=nMax))
    followersLocation = sapply(followers,location)
    list(users=followers, location=followersLocation)
  }

# get latitude and longitude from a location name
twitterUtil$findLatLon <- function(loc)
  {
    latlon = NA
    cont = NA

  # Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
  # hand coding the continents, not pretty
    continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
    continents[,1] = unique(world.cities[,2])
    continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
    continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
    continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
    continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
    continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
    continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
    continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
    continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
    continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
    continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
    continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
    continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
    continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
    continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
    continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
    continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
    continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
    continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
    continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
    continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
    continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
    continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
    continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
    continents[231:232,2] = c(2,1)


  # Get the first element of the location
 # firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
    firstElement = strsplit(loc,",")[[1]][1]
    if(is.na(firstElement)){firstElement="zzzzzzzzz"}

  # See if it is a city
    tmp = grep(firstElement,world.cities[,1],fixed=TRUE)
    tmp2 = grep(firstElement,state.name,fixed=TRUE)
    tmp3 = grep(firstElement,world.cities[,2],fixed=TRUE)

    if(length(tmp) == 1){
      latlon = world.cities[tmp,c(5,4)]
      cont = continents[which(world.cities[tmp,2]==continents[,1]),2]
    }else if(length(tmp) > 1){
      tmpCities = world.cities[tmp,]
      latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
      cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
    }else if(length(tmp2) == 1){
      latlon = c(state.center$x[tmp2],state.center$y[tmp2])
      cont = 3
    }else if(length(tmp3) > 0){
      tmpCities = world.cities[tmp3,]
      latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
      cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
    }
  
    return(list(latlon=latlon,cont=as.numeric(cont)))

  }

# get points along the great circle between two locations
twitterUtil$getGreatCircle <- function(userLL,relationLL)
  {
    tmpCircle = greatCircle(userLL,relationLL)
    start = which.min(abs(tmpCircle[,1] - userLL[1,1]))
    end = which.min(abs(tmpCircle[,1] - relationLL[1]))
    greatC = tmpCircle[start:end,]
    return(greatC)
  }

# can be performed with tm::stripWhitespace
#twitterUtil$trim <- function (x) gsub("^\\s+|\\s+$", "", x)

twitterUtil$commonReader <- function() {
        return(readTabular(mapping=
                    list(id="id", author="screenName",description="id", content="text", 
                         retweetCount="retweetCount", datetimestamp="created")))
}
attr(twitterUtil$commonReader, "help") <- "# return a reader, which will be used in creation of Corpus from tweets.df"

twitterUtil$setHeading <-function(x){
        #input_list <- as.list(substitute(list(...)))[-1L]
        #nwords <-  ifelse(input_list[["n"]] > 0, n, 3)
        #x <- input_list[[1]]
        nwords <- 3
        w <- paste0(head(unlist(strsplit(content(x), split = "\\s+")),nwords), collapse = " ")
        meta(x, tag="heading") <- w
        x
}
attr(twitterUtil$setHeading, "help") <- "for tm_map(): first 3 words become 'heading' metadata entity"

twitterUtil$setId <-function(x){
                w <- paste0(c(meta(x, tag="description"), meta(x, tag="author")),  collapse = "-")
                meta(x, tag="id") <- w
                x
}
attr(twitterUtil$setId, "help") <- "for tm_map(): 'id' metadata entity becomes 'description' plus 'screenName' - easier for DTMs"

twitterUtil$setDatestr <- function(x){
        #w <- as.POSIXct(meta(x, tag="datetimestamp"),origin = "1970-01-01",tz = "CET")
        w <- as.character(as.POSIXct(meta(x, tag="datetimestamp"),origin = "1970-01-01"))
        meta(x, tag="datetime") <- w
        x
}
attr(twitterUtil$setDatestr, "help") <- "for tm_map(): add new 'datetime' metadata entity displaying the datetime value in humanreadable form"

twitterUtil$author <- function(x){
        meta(x, tag="author")
}
attr(twitterUtil$author, "help") <- "show only author tag of a tm::PlainTextDocument"

twitterUtil$ratelimits <- function(x){
        rate.limit <- getCurRateLimitInfo()
        # print out all metrics that have been changed from their default values
        rate.limit[rate.limit$limit != rate.limit$remaining,]
}
attr(twitterUtil$ratelimits, "help") <- "twitter API endpoints: show which values are different from the default rate limit"


########################################
## Has to be last in file. 
# After executing this, funciton is part of global namespace, 
# so we do no longer need to fully qualify package name.

while("twitterUtil" %in% search())
        detach("twitterUtil")
attach(twitterUtil)