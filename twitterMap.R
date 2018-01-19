#########################################################################################
# This code is adapted from the following:
#
#  An R function to make a personalized map of people you follow and who follow you on twitter. 
#   R functions Copyright (C) 2011 Jeff Leek (jtleek@gmail.com), and the Simply Statistics Blog
#   (http://simplystatistics.tumblr.com, http://twitter.com/simplystats)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details, see <http://www.gnu.org/licenses/>.
#
#

require("twitteR")
require("maps")
require("geosphere") # for distCosine()


#####

source("twitterUtils.R")

# wrapper function
twitterMap <- function(userName,userLocation=NULL,fileName="twitterMap.pdf",nMax = 1000)
  {
  # Get location data
    paste0("Getting data for user ", userName, " from Twitter, this may take a moment.\n")
    usr = getUser(userName)
  
    if(is.null(userLocation)){
      userLocation = location(usr)
      userLocation = trim(userLocation)
      if(nchar(userLocation) < 2){stop("We can not find your location from Twitter")}
    }
    
    tmp=getFollowers(usr, n=nMax)
    followers=tmp$users
    followersLocation=tmp$location
    
    # Load the geographic data
    data(world.cities)
    data(us.cities)
    data(canada.cities)
    
    # Find the latitude and longitude of the user
    cat("Getting geographic (latitude/longitude) of Twitter users.\n")
    userLL <- findLatLon(userLocation)$latlon
    if(any(is.na(userLL))){stop("We can't find the latitude and longitude of your location from Twitter")}
    

  # Find the latitude and longitude of each of the followers/following
  # and calcualte the distance to the user
  
    followersLL = matrix(NA,nrow=length(followers),ncol=4)

    for(i in 1:length(followers)){
      if(length(followersLocation[[i]]) > 0){
        tmpLL = findLatLon(trim(followersLocation[[i]]))
        if(any(!is.na(tmpLL$latlon))){
          followersLL[i,] = c(unlist(tmpLL$latlon), distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
        }
      }
    }
    followersLL = followersLL[order(-followersLL[,3]),]
    followersLL = followersLL[!is.na(followersLL[,1]),]
    

    cat("Plotting results.\n")
    # Set up the colors
    cols = brewer.pal(7,"Set2")

    pdf(fileName,height=6,width=10)
    data(worldMapEnv)
    map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0)

    mtext(paste("@",userName," Follower Map",sep=""),col="lightgrey")
    nFollowers = dim(followersLL)[1]
    for(i in 1:nFollowers){
      greatC = getGreatCircle(userLL,followersLL[i,1:2])
      lines(greatC,col=cols[followersLL[i,4]],lwd=0.8)
    }
    
    legend(-180,0,legend = c(paste("Asia",sum(followersLL[,4]==1)),paste("Africa",sum(followersLL[,4]==2)),paste("N. America",sum(followersLL[,4]==3)),paste("S. America",sum(followersLL[,4]==4)),paste("Australia/N.Z.",sum(followersLL[,4]==5)),paste("Europe",sum(followersLL[,4]==6))),text.col=cols[1:6],bg="black",cex=0.75)
    mtext("Created by @simplystats twitterMap",side=1,adj=1,cex=0.8,col="grey")
    dev.off()
  }
# make maps

qry1 <-"natgeowild"
qry2 <- "nasa"

usr1 <- twitteR::getUser(qry1)
loc1 <- usr1$getLocation()
loc1 <- ifelse(loc1 == "", "London", loc1)

usr2 <- twitteR::getUser(qry2)
loc2 <- usr2$getLocation()
loc2 <- ifelse(loc2 == "", "London", loc2)

twitterMap(qry1, userLocation=loc1, nMax=500, file=paste0("01-", qry1, ".pdf"))
twitterMap(qry2, userLocation=loc2, nMax=500, file=paste0("02-", qry2, ".pdf"))

# a function to get latitude and longitude
# we'll use this in the distance analysis
getLatLon <- function(user, userLocation=NULL, n=500)
  {
    usr=getUser(user)
    tmp=getFollowers(usr,n=n)
    followers=tmp$users
    followersLocation=tmp$location
    userLL=findLatLon(userLocation)$latlon
    
    followersLL = matrix(NA,nrow=length(followers),ncol=4)
    
    for(i in 1:length(followers)){
      if(length(followersLocation[[i]]) > 0){
        tmpLL = findLatLon(trim(followersLocation[[i]]))
        if(any(!is.na(tmpLL$latlon))){
          followersLL[i,] = c(unlist(tmpLL$latlon), distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
        }
      }
    }
    followersLL = followersLL[order(-followersLL[,3]),]
    followersLL = followersLL[!is.na(followersLL[,1]),]
    followersLL
}

# get followers longitude and latitude for each team
natsLL=getLatLon(qry1, userLocation=loc1,n=1000)
yanksLL=getLatLon(qry2, userLocation=loc2, n=1000)

# make a data.frame that has distance of each follower, and the team they follow
distDF=data.frame(dist=c(yanksLL[,3],natsLL[,3]), team=rep(c(qry1, qry2),c(nrow(yanksLL),nrow(natsLL))))

# make a box plot of distance per team (notice formula syntax: "model distance as a function of team")
boxplot(dist~team, data=distDF)

# try log distance
boxplot(log(dist+1)~team, data=distDF, ylim=c(5,20))

# plot histogram of distances using lattice
lattice::histogram(~dist|team,data=distDF)
lattice::histogram(~log(dist+1)|team,data=distDF)

# linear regression model
fit=lm(log(dist+1)~team,data=distDF)
summary(fit)

# do a t-test for differences in mean log distance
testRes=t.test(log(subset(distDF,team==qry1)$dist+1),
  log(subset(distDF,team==qry2)$dist+1), alternative="greater")

# print result
testRes

# perform a KS test for differences in distribution of distances
testRes=ks.test(log(subset(distDF,team==qry1)$dist+1),
  log(subset(distDF,team==qry2)$dist+1), alternative="greater")

# print result
testRes
