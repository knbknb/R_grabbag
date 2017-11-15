library(SPARQL)
library(dplyr)
library(magrittr) # %<>%
library(stringr)
library(stringi)

replace_q <- function(x, extra="") {
    #       ifelse(extra == "", x,  str_replace_all(x, extra, "")) %>%
    str_replace_all(x, "\"", "") %>%
    str_replace_all("@[a-z]{2}", "") %>%
    str_replace_all("^<|>$", "")
}
attr(replace_q, "help") <- "Call: mydf %>% mutate_all(funs(replace_q))"


# https://query.wikidata.org/sparql?query=
endpoint1 <- "https://query.wikidata.org/sparql"
query1 <- '#Whose birthday is today?

SELECT ?entityLabel ?entityDescription    (YEAR(?date) as ?year)
WHERE {
BIND(MONTH(NOW()) AS ?nowMonth)
BIND(DAY(NOW()) AS ?nowDay)
?entity wdt:P569 ?date .
FILTER (Year(?date) >= 2000 && MONTH(?date) = ?nowMonth && DAY(?date) = ?nowDay)
SERVICE wikibase:label {        bd:serviceParam wikibase:language "en" .    }}
LIMIT 20'

qd1 <- SPARQL(endpoint1, query1)
(df1 <- qd1$results %>%
                mutate_all(funs(replace_q)))

#stri_enc_detect(qd1$results$entityLabel[1])
query2 <- '
# boreholes in wikidata. 3 results in 2017-11-15
SELECT * WHERE {
  ?bh wdt:P31 wd:Q502102.
  OPTIONAL { ?bh wdt:P625 ?coordinate_location. }
#  OPTIONAL { ?bh wdt:P18 ?image. }
  OPTIONAL { ?bh wdt:P2048 ?height. }
  OPTIONAL { ?bh wdt:P373 ?Commons_category. }
}
'

qd2 <- SPARQL(endpoint1, query2)
df2 <- qd2$results

(df2 %<>%
                mutate_all(funs(replace_q))
#                arrange(author_name, title) %>%
#                nest(-author_name, -title, -de_title)
)
