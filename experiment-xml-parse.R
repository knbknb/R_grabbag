library(XML)
library(purrr)
library(dplyr)

setwd("/home/knb/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag")
getwd()
doc.xml <- xmlTreeParse("experiment.xml", useInternalNodes = TRUE)
root <- xmlRoot(doc.xml)
xpathSApply(root, "//sample")
xmlName(root)
str(doc.xml)
names(root)
df1 <- bind_rows(map(xpathSApply(root, "//sample", xmlAttrs), .id="")
query <- doc.xml$doc$children$query_batch[["body"]]
