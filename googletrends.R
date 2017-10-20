if(! require("gtrendsR"))         install_github("knbknb/gtrendsR")

set.seed(1)
library(gtrendsR)
library(ggplot2)

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


energy_drinks_trend <- gtrends(c("red bull", "Bier", "Coca Cola"),  geo = c("DE"), hl="DE")
energy_drinks_trend <- energy_drinks_trend$interest_over_time
ggplot(energy_drinks_trend, aes(date, hits, color=keyword)) +
        geom_line() +
        ylab("Hits/Day") +
        xlab("Year") +
        ggtitle("Google trends: Energy Drinks Trend", subtitle = "Searches in Germany, last 5 years")


# soccer players with weird names
#soccer_trend <- gtrends(c("Kolasinac", "Aubameyang"), res="7d", geo = c("DE"))
#plot(soccer_trend)


#library(dtw)
#library(devtools)
#gtrendsR::
#gconnect("icdpdc@gmail.com", "*icdpdc*", verbose = TRUE)
#dbpedia_trend <- gtrends(c("dbpedia", "tensorflow"), res="7d")
#plot(dbpedia_trend)



####################################### DOES NOT WORK #########################################################
###
# cotton_trend <- gtrends(c("cotton", "satin", "velvet"), time = "now 7-d")
# d <- dist(t(cotton_trend$trend[, -(1:2)])) #, method="DTW")
# hc <- hclust(d)
#
#
# par(mfrow=c(1,2))
# plot(cotton~end, cotton_trend$trend, type="l", ylim=range(cotton_trend$trend[, -(1:2)]), col=3, ylim="")
# for (x in 4:ncol(cotton_trend$trend))
#         lines(x=cotton_trend$trend$end, y=cotton_trend$trend[, x], col=x)
#
# legend("topleft", names(cotton_trend$trend)[-(1:2)], lty=1, col=3:ncol(cotton_trend$trend))
#
# plot(hc)
# rect.hclust(hc, k=2)
#
#
# cutree(hc, k=2)
