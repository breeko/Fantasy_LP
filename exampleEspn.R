setwd("~/Documents/draftkings/")
source("espn.R")
source("utils.R")
source("history.R")
source("available.R")
source("solve.R")

library("ggplot2")

# HISTORIC ANALYSIS OF ESPN SCORES

h <- getLatestHistory()
h <- updateHistory(h,save=T)
h <- enrichHistory(h)
h <- h[h$year == 2018,]

# Review historic ESPN scores
espn <- getEspnHistory(2018, startPeriod = 1, endPeriod = 9, minPoints = 0)

write.csv(espn, "espn.csv")
espn <- read.csv("espn.csv")
espn <- enrichEspn(espn, h$displayName)

h <- merge(h, espn[c("displayName", "year", "week", "espn.dk.points")], by=c("displayName", "year", "week"), all.x=T)

espnCor <- h[!is.na(h$espn.dk.points),]

cor(espnCor$dk.points, espnCor$espn.dk.points)
# 0.6731

# Testing historical performance of ESPN model
lapply(seq(1,9), function(x) testWeek(espnCor, x))

# CHOOSE FROM AVIALABLE BASED ON ESPN SCORES
WEEKS_ID <- 22622
WEEK <- 10
YEAR <- 2018

a <- getLatestAvailable(WEEKS_ID)

ah <- availableToHistory(a, week = WEEK, year = YEAR, displayNames = h$displayName)

ah <- merge(ah, espn[c("displayName", "espn.dk.points")], by = "displayName")

ah$prediction <- ah$espn.dk.points

ah <- merge(ah, unique(h[c("displayName", "median")]), by="displayName", all.x = T)
# Filter out those that don't have a prediction
ahFiltered <- ah[!is.na(ah$prediction),]
ahFiltered$pos <- as.factor(ahFiltered$pos)

# solve
solve(ahFiltered)

