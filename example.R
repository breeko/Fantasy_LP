source("history.R")
source("solve.R")
source("available.R")

setwd("~/Documents/draftkings/")

WEEKS_ID <- 21720

h <- getLatestHistory()
a <- getLatestAvailable(WEEKS_ID)
ah <- availableToHistory(a, 3, 2018)
ah$prediction <- ah$salary / 500.0 + 10
sol <- solve(ah)
