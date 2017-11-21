library(SPARQL)
# endpoint = "http://live.dbpedia.org/sparql"
# #query= "select * where {<http://de.dbpedia.org/resource/Veit_Dietrich> ?p ?o}"
# query <- "SELECT *{dbpedia:Bertrand_Russell ?p ?o  }"
# qd=SPARQL(endpoint,query)
# df=qd$results

d <- SPARQL(url="http://services.data.gov.uk/reference/sparql",
            query="SELECT * WHERE { ?s ?p ?o . } LIMIT 10",
            ns=c('time','<http://www.w3.org/2006/time#>'))

if(is.data.frame(d$results)){
        res1 <- d$results
}


endpoint <- "http://semanticweb.cs.vu.nl/lop/sparql/"
q <-
        "SELECT *
   WHERE {
     ?event sem:hasPlace ?place .
     ?place eez:inPiracyRegion ?region .
   }"

# does not work
prefix <- c("lop","http://semanticweb.cs.vu.nl/poseidon/ns/instances/",
            "eez","http://semanticweb.cs.vu.nl/poseidon/ns/eez/")
res <- SPARQL(url = endpoint,query = q,ns=prefix, format = "xml")$results
pie(sort(table(res$region)),col=rainbow(12))
