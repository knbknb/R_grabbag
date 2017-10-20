library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# downloaded from http://georoc.mpch-mainz.gwdg.de/georoc/, knb 20170716

data <- read_csv("ANATOLIA-IRAN_BELT_-_PROTEROZOIC.csv", n_max = )
data %>% tail(5)
refs <- data %>%
tail(data)
glimpse(data)
summary(data)
data.tidy <- gather(data, key, value, -UNIQUE_ID, -`SAMPLE NAME` )
data.tidy2 <- data.tidy %>%

ggplot(data.tidy2, aes(key, value)) +
        geom_point()
