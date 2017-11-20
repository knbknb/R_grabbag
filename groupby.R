# https://stackoverflow.com/questions/47306757/check-if-means-break-certain-limits-r/
# knb 20171115
# kind of a fizzbuzz problem

library(dplyr)
df1 <- read.table(text = "V1  V2
1   125.8
1   128.4
1   129.0
1   121.0
2   125.2
2   127.0
2   130.4
2   124.6
3   121.8
3   126.8
3   133.8
3   144.5", header = TRUE)

warn_upper <- 130.4
action_upper  <- 132.1
warn_lower <- 123.6
action_lower  <- 121.9

df1 %>%
        group_by(V1) %>%
        summarize(smean = mean(V2)) %>%
        mutate(warn_hi = (smean - warn_upper)> 0 ,
               action_hi  = (smean - action_upper )> 0,
               warn_lo = (-1 * (smean - warn_lower))> 0 ,
               action_lo  = (-1 * (smean - action_lower ))> 0) %>%
        mutate(klass = if_else(warn_lo | action_lo |
                           warn_hi | action_hi, true = "shewart", false="" ))

