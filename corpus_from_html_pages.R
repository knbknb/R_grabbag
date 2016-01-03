library(tm)
library(XML)
#http://stackoverflow.com/questions/20971094/tm-combine-list-of-corpora
link <- c(
        "http://www.r-statistics.com/tag/hadley-wickham/",                                                      
        "http://had.co.nz/",                                                                                    
        "http://vita.had.co.nz/articles.html",                                                                  
        "http://blog.revolutionanalytics.com/2010/09/the-r-files-hadley-wickham.html",                          
        "http://www.analyticstory.com/hadley-wickham/"  
)               

create.corpus.from.url <- function(url.name){
        doc=tryCatch({htmlParse(url.name)}, error=function(e){})
                if(length(doc)){
                parag=xpathSApply(doc,'//p',xmlValue)
                if (length(parag)==0){
                        parag="empty"
                }
                cc=Corpus(VectorSource(parag))
                meta(cc,"link")=url.name
                return(cc)
        }
}


cc <- lapply(link,create.corpus.from.url)
ccc <- Filter(Negate(is.NullOb), cc)
# combine all into one corpus, with metadata conserved
cccc <- do.call(function(...) c(..., recursive = TRUE), ccc)
meta(cccc, tag = "link")

# remove whitespace from beginning and ending of strings, whitespace-only docs, empty docs
cccc <- tm_map(cccc, function(x) {content(x) <- gsub("\\s+$", "", content(x), perl=TRUE); x})
cccc <- tm_map(cccc, function(x) {content(x) <- gsub("^\\s+", "", content(x), perl=TRUE); x})
cccc <- tm_filter(cccc, function(x) any(grep("^\\s+$", content(x), perl=TRUE, invert=TRUE)))
cccc <- tm_filter(cccc, function(x) any(nchar(content(x)) > 0))

# special-purpose
cccc <- tm_filter(cccc, function(x) any(grep("^Download", content(x), invert = TRUE)))


meta(cccc, tag = "link")
tm_shown_content(cccc, ndoc = length(cccc))
