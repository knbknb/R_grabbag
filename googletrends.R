if(! require("gtrendsR"))         install_github("knbknb/gtrendsR")

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

ggplot(trend_reg, aes(keyword, hits)) +
        geom_point() +
        facet_wrap( ~location , ncol = 2) +
        ggtitle("Google trends: Deep Learning frameworks", subtitle = "Searches in Germany's 17 States, last 5 years")

library(gtrendsR)
library(ggplot2)

energy_drinks_trend <- gtrends(c("red bull", "Bier", "Coca Cola", "Fanta"),  geo = c("DE"), hl="DE")
energy_drinks_trend <- energy_drinks_trend$interest_over_time
ggplot(energy_drinks_trend, aes(date, hits, color=keyword)) +
        geom_line() +
        ylab("Hits/Day") +
        xlab("Year") +
        ggtitle("Google trends: Soft Drinks", subtitle = "Searches in Germany, last 5 years")


# soccer players with weird names #kolasinac
soccer_trend <- gtrends(c("Neymar", "Aubameyang", "Lewandowski"), geo = c("DE"), hl="DE")
soccer_trend <- soccer_trend$interest_over_time
ggplot(soccer_trend, aes(date, hits, color=keyword)) +
        geom_line() +
        ylab("Hits/Day") +
        xlab("Year") +
        ggtitle("Google trends: Soccer Players", subtitle = "Searches in Germany, last 5 years")


#library(dtw)
#library(devtools)
#gtrendsR::
#gconnect("icdpdc@gmail.com", "*icdpdc*", verbose = TRUE)
#dbpedia_trend <- gtrends(c("dbpedia", "tensorflow"), res="7d")
#plot(dbpedia_trend)

