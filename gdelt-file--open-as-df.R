# GDELT is the largest, most comprehensive, and highest resolution open database of human society ever created.
# Creating a platform that monitors the world's news media from nearly every corner of every country
# in print, broadcast, and web formats, in over 100 languages, every moment of every day .
#
# Conflicts in germany January 2016
#
# Your GDELT EVENT Exporter results are now ready for download:
#
#         http://data.gdeltproject.org/analytics_user/20160118082001.2278.events.csv
#
# Your search criteria used to construct this graph:
#
#         Start Date = 01/17/2016
# End Date = 01/18/2016
# Actor1 Country: DEU
# Actor1 Type:
#         Actor2 Country:
#         Actor2 Type:
#         Event Code: 14
# Event Quad Class:
#         Event Country: GM
# Weighting: NUMEVENTS
library(ggmap)
#library(maps) # no germany, only usa, it, fr, nz
# library(RgoogleMaps)
# ger_map <- RgoogleMaps::GetMap(center=c(mean(df1$Actor1Geo_Long), mean(df1$Actor1Geo_Long)), zoom=13)



# /home/knb/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag
setwd("/home/knb/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag")
df1 <- read.csv("20160118-082001.2278.events.csv")
# str(df1)
# lapply(df1$SOURCEURL, function(x){
#         system(paste0("firefox ", x))
# })
df1$ActionLocation <- stringr::str_replace(df1$ActionGeo_FullName, pattern = ",.+", replacement = "")
df1$Actor2Name <- as.factor(df1$Actor2Name )
#library(ggplot2)
ger_map = get_map("Germany", zoom=6)
# Build the map
ggmap(ger_map, aes(long, lat, group=group)) +
        geom_point(data=df1, aes(x=ActionGeo_Long, y=ActionGeo_Lat,col=Actor2Name), size=2)
        # geom_label(data = df1, aes(label= ActionLocation),
        #            size = 4, fontface = "bold", fill = "grey90", col = "#E41A1C")

