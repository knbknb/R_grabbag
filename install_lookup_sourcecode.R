# lookup R package
# Lookup R full function definitions, including compiled code, S3 and S4 methods.
# install it, configure it
# https://github.com/jimhester/lookup#readme
# knb 20161205
devtools::install_github("jimhester/lookup")

# gh::gh("/rate_limit") can be used to query your current usage and limits.
gh::gh("/rate_limit")

# lookup makes heavy use of the GitHub API,
# which has a rate limit of 60 requests per hour when unauthenticated.
# You can create a Personal access token with no scope,
# which will increase your limit to 5000 requests per hour.
library(RSQLite)
dbGetQuery
