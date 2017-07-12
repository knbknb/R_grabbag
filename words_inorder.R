# http://stackoverflow.com/questions/40806213/create-alphabetically-sorted-word-cloud
library(ggplot2)
library(ggmap) # theme_nothing
library(purrr)
dat <- read.delim("words_inorder.txt", sep = "",
                  header=FALSE, stringsAsFactors = FALSE)
dat <- dat[order(dat[,1]),]
p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))

p + geom_text()
# Avoid overlaps
p + geom_text(check_overlap = TRUE)
# Labels with background
p + geom_label()
# Change size of the label
p + geom_text(size = 10)
