if(! require("gtrendsR"))         install_github("PMassicotte/gtrendsR")

set.seed(1)
library(gtrendsR)
library(ggplot2)
library(dplyr)

deeplearn_trend <- gtrends(c("theano", "tensorflow", "keras"),  geo = c("DE"), hl="DE")
#deeplearn_trend.xts <- as.xts(x= deeplearn_trend$trend[,2:length(deeplearn_trend$trend)], index=deeplearn_trend$start)
trend <- deeplearn_trend$interest_over_time
ggplot(trend, aes(date, hits, color=keyword)) +
        geom_line() +
        ylab("Hits/Day") +
        xlab("Year") +
        ggtitle("Google trends: Deep Learning frameworks", subtitle = "Searches in Germany, last 5 years")


trend_reg <- deeplearn_trend$interest_by_region
trend_reg <- trend_reg %>%
        group_by(location, keyword) %>%
        summarise(hits = sum(hits)) %>%
        ungroup()

ggplot(trend_reg, aes(location, hits)) +
        geom_col() +
        facet_wrap( ~keyword , nrow = 3) +
        ggtitle("Google trends: Deep Learning frameworks", subtitle = "Searches in Germany's 17 States, last 5 years") +
        theme(axis.text.x=element_text(angle=45,hjust=1))

#library(dtw)
#library(devtools)
#gtrendsR::
#gconnect("icdpdc@gmail.com", "*icdpdc*", verbose = TRUE)
#dbpedia_trend <- gtrends(c("dbpedia", "tensorflow"), res="7d")
#plot(dbpedia_trend)

