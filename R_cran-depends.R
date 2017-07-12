AP <- available.packages(contrib.url("http://cran.r-project.org"))

# which packages depend on "tm"
rownames(AP[which(grepl("tm", as.data.frame(AP)$Depends)), ])


## more concise count
## length(grep("Rcpp", AP[,"Depends"]))
## sum(grepl("Rcpp", AP[,"Depends"]))

# depends = data.frame(pattern= c("Rcpp", "RcppArmadillo",
#                                 "RcppEigen", "RcppGSL", "rJava"))

depends = data.frame(pattern= c("tm", "ggplot2", "dplyr", "parallel"))

# I don't understand this code
library(plyr)
pl <- mlply(depends,  grep, x = AP[,"Depends"])

(arr <- arrange(ldply(pl, length), -V1))


arr[arr$pattern =="tm",]

