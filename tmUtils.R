########################################
## Put everything into an environment, to not pollute global namespace

tmUtil <- new.env()

tmUtil$tm_freqterms <- function(tdm, lowfreq=1, topn=-1){
        stopifnot(inherits(tdm, c("DocumentTermMatrix", "TermDocumentMatrix")), 
                  is.numeric(lowfreq), is.numeric(topn))
        if (inherits(tdm, "DocumentTermMatrix")) 
                tdm <- t(tdm)
        if(topn < 0)
                topn <- nrow(tdm)
        freqterms <- findFreqTerms(tdm, lowfreq = lowfreq)
        vec <- apply(tdm[ rownames(tdm) %in% freqterms, ], 1, sum)
        
        topn <- ifelse(topn > length(vec), length(vec), topn)
        vec <- sort(vec[vec > 0], decreasing = TRUE)[1:topn]
        vec[!is.na(names(vec))]
}
#tmUtil$removeNonAlnum<-function(x) gsub("[[^:alnum:]]","",x)

tmUtil$tm_removeNonAlnum <- function(x){
        doc <- tryCatch ({
                gsub("\\s\\W+|\\W+\\s"," ",x, perl = TRUE)
        }, error=function(e) e)
        PlainTextDocument(doc)
}
attr(tmUtil$tm_removeNonAlnum, "help") <- "Remove NonAlphanumeric Characters from Beginning or end of a word within a PlainTextDocument"


tmUtil$tm_removeStopwords <- function(...){
        doc <- tryCatch ({
                removeWords(...)
        }, error=function(e) e)
        PlainTextDocument(doc)
}
attr(tmUtil$tm_removeStopwords, "help") <- "Remove a Vector of stopwords from a PlainTextDocument.\n
call: tm_map(corpus, content_transformer(tm_removeStopwords), myStopwords)"


tmUtil$tm_convertToUTF8 <- function(x) {
        doc <- tryCatch({
                iconv(enc2utf8(x), sub = "byte")
        },error=function(e){warning(e); return("convertToUTF8(): cannot convert text")
        })
        PlainTextDocument(doc)
        #doc
}

tmUtil$tm_shown_meta <- function(corpus, ndoc=1, tag="*") {
        if(exists("tm_randn")){
                randn <<- tm_randn
        } else {
                shown <- ndoc
                shown <- min(shown, length(corpus))
                randn <- sample(x=length(corpus), size=shown)       
        }
        if(tag != "*"){
                sapply(randn, function(i) {meta(corpus[[i]], tag=tag)})
        } else {
                sapply(randn, function(i) {meta(corpus[[i]])})
        }
}
attr(tmUtil$tm_shown_meta, "help") <- "sample n docs from Corpus and show their metadata sections"

tmUtil$tm_shown_content <- function(corpus, ndoc=1) {
        if(exists("tm_randn")){
                randn <<- tm_randn
        } else {
                shown <- ndoc
                shown <- min(shown, length(corpus))
                randn <- sample(x=length(corpus), size=shown)       
        }
        # for unknown reasons, sometimes the text is too corrupt to be shown
        tryCatch({sapply(randn, function(i) {content(corpus[[i]])})}, error=function(e){warning(e); return("cannot show content:")})
        #sapply(randn, function(i) {content(corpus[[i]])})
}
attr(tmUtil$tm_shown_content, "help") <- "sample n docs from Corpus and show their text content"



########################################
## Has to be last in file. 
# After executing this, funciton is part of global namespace, 
# so we do no longer need to fully qualify package name.

while("tmUtil" %in% search())
        detach("tmUtil")
attach(tmUtil)