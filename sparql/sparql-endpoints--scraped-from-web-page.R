# dir: "/home/knb/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag"
# knb 20171116

library(httr)
library(dplyr)
library(rvest)
library(purrr)

url  <- 'https://www.w3.org/wiki/SparqlEndpoints'  # unmaintained
url2 <- 'http://sparqles.ai.wu.ac.at/availability' # 565 links in Nov 2017

res <- GET(url,
           query = list())

res2 <- GET(url2,
           query = list())

spq <- content(res, as = 'text', encoding = 'utf-8') %>%
        read_html()

spq2 <- content(res2, as = 'text', encoding = 'utf-8') %>%
        read_html()

#Next, we target and extract the table node with the report:
tab <- html_nodes(spq, "table")[1] %>%
        html_table(header = TRUE) %>%
        as.data.frame() %>%
        as_tibble()

tab2.links <- html_nodes(spq2, "table")[1]
tab2.l <- map(tab2.links, function(x){
        txt  <- html_nodes(x, xpath=".//td/a") # assumes only 1 column has links
        href <- html_text(html_nodes(x, xpath=".//td/a/@href"))
        link <- gsub("^/endpoint\\?uri=", "", href, perl=TRUE)

        list(href= href,
             link = map_chr(link, URLdecode),
             linktext= ifelse(is.na(txt),"", html_text(html_nodes(txt, xpath=".//text()"))) %>% tolower())
}) %>% map_df(as_tibble)

tab2 <- html_nodes(spq2, "table")[1] %>%
        html_table(header = TRUE) %>%
        as.data.frame() %>%
        as_tibble()

tab2 <- tab2 %>%
        select(-Var.1) %>%
        bind_cols(tab2.l)

tab3 <- tab2 %>%
        select(-href) %>%
        filter(Uptime.Last.7.days != "0%") %>%
        arrange(linktext)

