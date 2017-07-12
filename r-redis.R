library(rredis)
library("tools")
library(jsonlite)
vigSrc = list.files(pattern = "Rnw$",
                    system.file("doc", package = "rredis"),
                    full.names = TRUE)
vigSrc
#for (v in vigSrc) Stangle(v) # empty file

redisConnect("139.17.114.3")
redisSet("x",rnorm(5))
redisGet("x")
#[1] 0.808448325 0.341482747 -0.728739322 -0.105507214 -0.002349064
redisGet("shell")
lasjson <- redisGet("lasobj")
lasstr <- jsonlite::fromJSON(lasjson)
lasstr[[1]]
