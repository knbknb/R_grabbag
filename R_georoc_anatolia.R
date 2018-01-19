library(readr)
library(tidyverse)
# downloaded from http://georoc.mpch-mainz.gwdg.de/georoc/, knb 20170716

data <- read_csv("data/ANATOLIA-IRAN_BELT_-_PROTEROZOIC.csv", n_max = 100)
data <- data %>%
        select(-X172) %>%
        unite(GEOL_AGE, GEOL., AGE, sep = " ", remove = TRUE)
# rename some columns
names(data) <- gsub("\\s+", "_", names(data))
data <- data %>%
        mutate(ROCK_NAME = str_trim(str_replace_all(ROCK_NAME, "\\[8520\\]", "")),
               SAMPLE_NAME = str_trim(str_replace_all(SAMPLE_NAME, "\\[8520\\]", "")),
               MATERIAL = str_trim(str_replace_all(MATERIAL, "\\[8520\\]", "")),
               GEOL_AGE = str_trim(str_replace_all(GEOL_AGE, "\\[8520\\]", "")),
               ROCK_NAME = str_replace_all(ROCK_NAME, "\\[\\d+\\]", ""),
               UNIQUE_ID = as.factor(UNIQUE_ID),
               MATERIAL = as.factor(MATERIAL),
               ROCK_TYPE = as.factor(ROCK_TYPE),
               ROCK_NAME = as.factor(ROCK_NAME),
               SAMPLE_NAME = as.factor(SAMPLE_NAME),
               LAND_OR_SEA = as.factor(LAND_OR_SEA),
               TECTONIC_SETTING = as.factor(TECTONIC_SETTING),
               GEOL_AGE = as.factor(GEOL_AGE))

wtpct_idx <- str_detect(names(data), "WT")
#wtpct_idx <- c(1:14,23,24)
wtpct_vals <- names(data)[wtpct_idx]
wtpct_vals_pretty <- wtpct_vals %>%  gsub(x = ., "^(.)([A-Z])", "\\1\\L\\2", perl=TRUE)
names(data)[wtpct_idx] <- wtpct_vals_pretty

ppm_idx <- str_detect(names(data), "PPM")
ppm_vals <- names(data)[ppm_idx]
ppm_vals_pretty <- ppm_vals %>% tolower() %>% gsub(x = ., "^([a-z])", "\\U\\1", perl=TRUE)
names(data)[ppm_idx] <- ppm_vals_pretty

# used to buld the -exclusion list in gather() call below
# gl <- capture.output(glimpse(data) )
# (gl <- substr(gl, 3, 200))
# (gl2 <- gsub(gl, pattern = "\\s.*$", replacement = "", perl=TRUE))
# cat(gl2)

data_tidy <- data %>% gather(
  k, v,
  -CITATIONS,
  -TECTONIC_SETTING, -LAND_OR_SEA,
  -LOCATION, -LOCATION_COMMENT,
  -LATITUDE_MIN, -LATITUDE_MAX,
  -LONGITUDE_MIN, -LONGITUDE_MAX,
  -ELEVATION_MIN, -ELEVATION_MAX,
  -SAMPLE_NAME, -ROCK_NAME,
  -`MIN._AGE_(YRS.)`, -`MAX._AGE_(YRS.)`, -GEOL_AGE,
  -ERUPTION_DAY, -ERUPTION_MONTH, -ERUPTION_YEAR,
  -ROCK_TEXTURE, -ROCK_TYPE,
  -DRILL_DEPTH_MIN, -DRILL_DEPTHAX,
  -ALTERATION, -MINERAL, -MATERIAL,
  -UNIQUE_ID
)

data_tidy2 <- data_tidy %>% mutate(v = as.numeric(v), k=as.factor(k))
summary(data_tidy2)

data_tidy_wtpct <- data_tidy2 %>%
        filter(k %in% wtpct_vals_pretty)

data_tidy_ppms <- data_tidy2 %>%
        filter(k %in% ppm_vals_pretty)


data_tidy_wtpct_sub <- data_tidy_wtpct %>%
        select(UNIQUE_ID, LAND_OR_SEA, TECTONIC_SETTING, ROCK_NAME, k, v) %>%
        na.omit()


ggplot(data_tidy_wtpct_sub, aes(k, y=v, group=ROCK_NAME, color=ROCK_NAME)) +
        geom_boxplot() +
        theme(legend.position = "top") +
        facet_wrap(~k, nrow = 3, scales = "free")

