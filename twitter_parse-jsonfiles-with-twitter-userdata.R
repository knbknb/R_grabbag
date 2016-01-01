library(twitteR)
#setwd("/mnt/hd2tb/Documents/coursera/datascience/getting_data/twitter")
library(jsonlite)
#library(rjson)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
###################################################################
## a simple helper function for testing and setting the working directory
# input: a dirname string
check_directory <- function(dirname){
        rcflag = FALSE
        if (file.exists(dirname)){
                message(sprintf("Good: Subdir '%s'' found. Assuming it contains *.txt data files with the Samsung dataset.", dirname))
        } else {
                ## Set home directory
                d = "/home/knut/Documents/coursera/datascience/getting_data/twitter"
                if(! file.exists(d)){
                        warning(sprintf("Directory '%s' not found. Enter full path to its parent directory on your computer.", d ), immediate.=TRUE)
                        n <- FALSE;
                        while(! n){
                                cat("Path (Ctrl-C to exit):")
                                d <- readLines(con="stdin", n=1)
                                n <- ifelse(! file.exists(d),FALSE, TRUE)
                                #if(n == "0"){break}  # breaks when hit enter
                                
                        }
                        setwd(d)
                        warning(paste0("Now inside working directory", getwd()), immediate.=TRUE)
                } else {
                        setwd(d)
                }
                rcflag = TRUE
        }
        
        
        if(! file.exists(dirname)){      
                stop(paste0("Subdir '", dirname, "' not found. Must already exist and contain *.txt data files. Create the directory and put unzipped files there."))
                
                #message("You can run helper script download-and-extract.R to download and unzip necessary data files")
                #source("download-and-extract.R")
                #message(paste0("Subdir ", dirname, " not found,. Must already exist and contain data files."))
                #message("You can run helper script download-and-extract.R to download and unzip necessary data files")
                #source("download-and-extract.R")
        }
        rcflag
        
}

## We assume file has been downloaded and extracted already
subdir <- "twusers"
dataset_found <- check_directory(subdir)
###read in all filenames (as full paths) extracted from the zip-file, will filter this list of strings many times, later on.
datafile.list <- list.files(subdir, full.names=TRUE, recursive=TRUE, pattern="*.json" )

a <- fromJSON("twusers/a_freimanis/twitterusers.Atis_Freimanis--AFreimanis.json", flatten=TRUE)
read_data <- function(df, colclasses=NA, nRow=NA){
        do.call("rbind",lapply(df ,
                               FUN=function(files){
                                       s <- ""
                                       tryCatch({
                                              s <- fromJSON(files, flatten=TRUE)
                                              s <- s[,c("name", "screen_name", "url", "lang", "friends_count", "followers_count", "statuses_count")] 
                                               #iconv(s$S, from="latin1", to="")
                                              #c(s$name, $screenname, $url)
                                              s
                                              #jsonlite::fromJSON(jsonlite::toJSON(s))
                                       }, finally = {message(files);})
                                       }))
}



# find column types  , sample 1 file
n <- 1
datafiles.sampled <- head(datafile.list,n)
#data.sampled <- read_data_fwf(datafiles.sampled)
#to get datatypes of columns, sample 2 rows from 1 file first
nr <- 2
data.sampled <- read_data(datafiles.sampled, nRow=2)
classes <- sapply(data.sampled, class)

n <- 2 
#n <- length(datafile.list)
datafile.use <- head(datafile.list,n)
datafile.use 
#data <- read_data_fwf(datafile.use)
jsondata <- read_data(datafile.use, classes)



