set.seed(1)
library(gtrendsR)
#library(dtw)
#library(devtools)
#install_github("pmassicotte/gtrendsR")

gconnect("icdpdc@gmail.com", "*icdpdc*", verbose = TRUE)
# soccer players with weird names
soccer_trend <- gtrends(c("Kolasinac", "Aubameyang"), res="7d", geo = c("DE"))
plot(soccer_trend)

dbpedia_trend <- gtrends(c("dbpedia", "tensorflow"), res="7d")
plot(dbpedia_trend)

deeplearn_trend <- gtrends(c("theano", "torch", "tensorflow", "keras"), res="7d", geo = c("DE"))
plot(deeplearn_trend)
###
cotton_trend <- gtrends(c("cotton", "satin", "velvet"), res="7d")
d <- dist(t(cotton_trend$trend[, -(1:2)])) #, method="DTW")
hc <- hclust(d)


par(mfrow=c(1,2))
plot(cotton~end, cotton_trend$trend, type="l", ylim=range(cotton_trend$trend[, -(1:2)]), col=3, ylim="")
for (x in 4:ncol(cotton_trend$trend))
        lines(x=cotton_trend$trend$end, y=cotton_trend$trend[, x], col=x)

legend("topleft", names(cotton_trend$trend)[-(1:2)], lty=1, col=3:ncol(cotton_trend$trend))

plot(hc)
rect.hclust(hc, k=2)


cutree(hc, k=2)
