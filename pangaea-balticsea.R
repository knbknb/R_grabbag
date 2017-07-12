library(downloader)

library(dplyr)
library(tidyr)
library(ggplot2)

library(RNetCDF)
#install.packages("RNetCDF")
# higher-level API
library(ncdf4)
#install.packages("ncdf4")

library(lubridate)
library(gridExtra)
library(ggthemes)
library(purrr)
library(cowplot) # white theme
library(GGally) ## ggpairs
#??rectGrob
#grid.arrange(rectGrob(), rectGrob())
## Not run:


getwd()
setwd("/home/knb/git/_my/R_one-offs/R_grabbag")

g2ppm <- 0.684

# convert gram/m3 to ppm, see http://how-it-looks.blogspot.de/2010/07/how-to-convert-to-and-from-parts-per.html
# Download this: (just metadata)
# Lammert, A et al. (2013): Long-term eddy-covariance measurements from FINO2 platform
# above the Baltic Sea (NetCDF format). doi:10.1594/PANGAEA.808714
#
zf <- "balticsea-data.tsv"
if(! file.exists(zf)){
        download("http://doi.pangaea.de/10.1594/PANGAEA.808714?format=textfile", zf)
}

con <- file(zf,open="r")
lines <- readLines(con)
skipn <- match("*/", lines) #gets the row index of the close comment

filelist <- read.tsv(zf, skip = skipn, stringsAsFactors = FALSE, header = TRUE )

n <- 1
i <- 1 # data from 2008
for(i in 1:n){
        netcdf.file <- filelist[i, "File.name"]
        if(! file.exists(netcdf.file)){
                download.file(url = filelist$URL.file[i], destfile = netcdf.file)
        }
}

# low-level, API,
conn <- RNetCDF::open.nc(netcdf.file)
dat <- read.nc(conn) # the only higher-level function, returns nested list
close.nc(conn)

# plot a single data series

co2_curve <- function(idx=1){
        co2 <- data.frame(day=fast_strptime(dat$isotime,  "%Y-%m-%dT%H:%M:%S"), co2=dat$CO2[idx,])
        summary(co2)
        colnames(co2)
        co2$hour <- hour(co2$day)
        ggplot(co2, aes(x=day, y=co2*g2ppm * 1000)) +
                geom_point(alpha=0.4) +
                geom_line(alpha=0.4) +
                theme_tufte(base_family="Helvetica") +
                geom_smooth(method="loess",se = FALSE) +
                #facet_wrap(~hour) +
                xlab("day in 2008") +
                ylab("PPM CO2")
}

plots <- map(1:2, co2_curve)
plot_grid(plotlist = plots, labels = c("", ""), rows = 2, cols = 1)

# read in all, keep only valid
dat.slim  <-  dat
dat.slim[ c("flag", "lon", "lat", "time", "base_time", "height")] <- NULL
#dat.slim$isotime <- fast_strptime(dat.slim$isotime,  "%Y-%m-%dT%H:%M:%S")

dat.slim2 <- lapply(dat.slim[2:length(dat.slim)], function(el) el[1,])
dat.slim2 <- lapply(c(dat.slim[1], dat.slim2), cbind) # dat.slim[1:2] %>% lapply(as.data.frame) %>% bind_rows()
dat.slim2 <- as.data.frame(dat.slim2)
dat.slim2 <- dat.slim2[which(dat.slim2$valid > 0),]
dat.slim2[ , c("valid") ] <- NULL
dat.slim3 <- dat.slim2
#summary(dat.slim2)
colnames(dat.slim2)

nc = ncdf4::nc_open(netcdf.file)
#variables = names(nc[['var']])
for (i in 2:(nc$nvars)){
        varinfo <- nc[['var']][[i]]

        cat("        ", varinfo$name, " ", varinfo$units, sep = "")
        if(varinfo$name %in% colnames(dat.slim2) & nchar(varinfo$units )> 0)
                colnames(dat.slim2)[which(colnames(dat.slim2) == varinfo$name)]<- paste0(varinfo$name , " [", varinfo$units, "] ", varinfo$longname)

}
colnames(dat.slim2)

co2.sensor.data <- dat.slim2 %>%
        gather(Param, Value, 2:(ncol(dat.slim2)))


# Plot each graph with appropriately scaled y-axis and
# as a function of isodate
pl <- lapply(colnames(dat.slim2)[-c(1)], function(n){
        #what <- which(grepl(n, co2.sensor.data[, "Param"], perl = TRUE))
        #what <- which(grepl(n, co2.sensor.data["Param" == n,], perl = TRUE))
        co2.sensor.data.what <- co2.sensor.data[co2.sensor.data$Param == n,]
        co2.sensor.data.what %>%
                ggplot(aes(x=day(isotime), y=Value)) +
                #geom_boxplot(aes(x=day(isotime), y=Value), notch = TRUE, position=position_dodge(0.8)) +
#                geom_point() +
                geom_jitter(size = 0.5, width = 0.2) +
                geom_line(stat="summary", fun.y="median", color="red", size=0.3) +
                ylab(label=NULL) +

                ggtitle(label = n) +
#                facet_wrap(~Param, nrow=length(unique(co2.sensor.data.what$Param))) +
                theme(legend.position="top")
})


ml <- marrangeGrob(pl, nrow=5, ncol=1)
## interactive use; open new devices
ml


unique(co2.sensor.data$Param)
ggplot(co2.sensor.data[co2.sensor.data$Param == "T [K] Air temperature from USAT", ] , aes(x=day(isotime), y=Value)) +
        geom_boxplot(fill="#2b2b2b", alpha=0.25, width=0.75, size=0.25) +
        scale_x_discrete(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        #coord_flip() +
        theme_tufte(base_family="Helvetica") +
        theme(axis.ticks=element_blank()) +
        labs(x=NULL, y=NULL)


ggsave("multipage-2008-co2sensordata-balticsea.pdf", ml)

# make complex plot matrix (xyplots, correlations)

dat.slim2$isoday <- day(fast_strptime(as.character(dat.slim2$isotime),  "%Y-%m-%dT%H:%M:%S"))
dat.slim3 <- dat.slim2
dat.slim3[, "isotime"] <- NULL
var_ <- colnames(dat.slim3[which(grepl("var_", colnames(dat.slim3)))])
mycolumns <- setdiff(colnames(dat.slim3), c("dd", "u", "v", "w", var_))

mycolumns.ok <- make.names(mycolumns)
colnames(dat.slim3)[which(colnames(dat.slim3) %in% mycolumns)] <- mycolumns.ok
ggpairs(dat.slim3[, mycolumns.ok[6:8]], size=0.1)


