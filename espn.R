setwd("~/Documents/draftkings/")

library('RCurl')
library(rvest)
source("utils.R")
source("mappings.R")

URL_BASE <- "http://games.espn.com/ffl/tools/projections?&startIndex=INDEX&scoringPeriodId=PERIOD_ID&seasonId=YEAR"
URL_BASE_DST <- "http://games.espn.com/ffl/tools/projections?&scoringPeriodId=PERIOD_ID&seasonId=YEAR&slotCategoryId=16"

getEspnDstTable <- function(url){
  out <- getURL(url)
  html <- read_html(out)
  out <- xml_find_first(html,'.//table[contains(@class,"playerTableTable")]')
  out <- data.frame(html_table(out))
  colnames(out) <- unlist(out[2,]) # use first row as header
  out <- out[-c(1,2),]
  out <- out[!grepl("BYE", out$OPP),]
  out <- convertToNumeric(out)
  points = espnDstToPoints(out)
#  out$dk.points.espn <- points
  return(out)
}

getEspnTable <- function(url) {
  out <- getURL(url)
  html <- read_html(out)
  tableText <- xml_find_first(html,'.//table[contains(@class,"playerTableTable")]')
  tableText <- html_table(tableText)
  colnames(tableText) <- unlist(tableText[1,]) # use first row as header
  tableText <- tableText[-1,]
  if (nrow(tableText) > 0) {
    player <-data.frame(do.call('rbind', strsplit(as.character(tableText$`PLAYER, TEAM POS`),', ',fixed=TRUE)), stringsAsFactors = F)[1]
    teamPos <-data.frame(do.call('rbind', strsplit(as.character(tableText$`PLAYER, TEAM POS`),', ',fixed=TRUE)), stringsAsFactors = F)[2]
    teamPos <- lapply(teamPos, function(x) gsub("[^a-zA-Z,]+", " ", x))
    team <- lapply(teamPos, function(x) gsub("[^a-zA-Z].*", "", x))
    pos <- lapply(teamPos, function(x) gsub(".*[^a-zA-Z]", "", x))
    
    tableText["player"] <- player
    tableText["team"] <- team
    tableText["pos"] <- pos
  }
  return(as.data.frame(tableText))
}

getEspnHistory <- function(year=2018, startPeriod=1, endPeriod=17, minPoints=1){
  # Returns espn projection history for all periods in a given year. Filtered by minimum score 
  curPeriod <- startPeriod
  curIndex <- 0
  history <- data.frame()
  while(T){
    curUrl <- gsubs(c("YEAR", "PERIOD_ID", "INDEX"), c(year, curPeriod, curIndex), URL_BASE)
    print(curUrl)
    curTable <- getEspnTable(curUrl)
    curTable <- curTable[curTable$PTS >= minPoints,]
    if (nrow(curTable) == 0){
      if (curIndex == 0 | curPeriod >= endPeriod){
        break
      } else{
        curIndex <- 0
        curPeriod <- curPeriod + 1
      }
    } else{
      curTable["YEAR"] <- year
      curTable["PERIOD"] <- curPeriod
      history <- rbind(history, curTable)
      curIndex <- curIndex + 40
    }
  }
  history <- convertToNumeric(history)
  return(history)
}

getEspnDstHistory <- function(year=2018, startPeriod=1, endPeriod=17){
  curPeriod <- startPeriod
  history <- data.frame()
  while(curPeriod <= endPeriod){
    curUrl <- gsubs(c("YEAR", "PERIOD_ID"), c(year, curPeriod), URL_BASE_DST)
    print(curUrl)
    curTable <- getEspnDstHistory(curUrl)
    curTable["YEAR"] <- year
    curTable["PERIOD"] <- curPeriod
    if (nrow(curTable) == 0) break
    history <- rbind(history, curTable)
    return(history)
    curPeriod <- curPeriod + 1
  }
  return(history)
}

temp <- getEspnDstHistory(startPeriod = 5, endPeriod = 5)

espnToDkPoints <- function(espn){
  passing <- (0.04 * espn$yds) + (4.0 * espn$td) + (-1.0 * espn$int) + (3.0 * espn$yds >= 300)
  rushing <- (4.0 * espn$td.1) + (0.1 * espn$rush) + (3.0 * espn$rush >= 100)
  receiving <- (6.0 * espn$td.2) + (0.1 * espn$yds.2) + (3.0 *espn$yds.2 >= 100) + (1.0 * espn$rec)
  # TODO: Figure out how to convert espn defense scores
  return(passing + rushing + receiving)
}

espnDstToDkPoints <- function(espn){
  out <- cbind(espn)
  colnames(out) <- tolower(colnames(out))
  sacks <- 1.0 * out$sck
  interceptions <- 2.0 * out$int
  fumbleRecovery <- 2.0 * out$fr
  blocks <- 6.0 * out$blk
  td <- 6.0 * out$td
  safety <- 2.0 * out$sfty
  pointsAllowed <- {
    if (out$pa == 0) 10.0
    else if (out$pa <= 6) 7
    else if (out$pa <= 13) 4
    else if (out$pa <= 20) 1
    else if (out$pa <= 27) 0
    else if (out$pa <= 34) -1
    else -4
  }
  return(sacks + interceptions + fumbleRecovery + blocks + td + safety + pointsAllowed)
}

enrichEspn <- function(df, displayNames){
  out <- rbind(df)
  colnames(out) <- tolower(colnames(out))
  out$espn.dk.points <- espnToDkPoints(out)
  out$displayname <- NULL
  out$week <- out$period
  out$displayName <- gsub("\\*", "", out$player)
  out$displayName <- gsub(" D/ST.*", "", out$displayName)
  out$displayName <- unlist(lapply(out$displayName, function(x) softMatch(x, displayNames)))
  out$displayName <- unlist(lapply(out$displayName, function(x){mapOrDefault(x, availableToHistoryName, x)}))
  return(out)
}

testWeek <- function(combined, week){
  requiredFields <- c("week", "espn.dk.points", "dk.salary", "dk.points")
  if (!all(requiredFields %in% names(combined))) {
    stop(paste("Invalid input. Must have fields:", paste(requiredFields, collapse = ", ")))
  }
  week <- espnCor[combined$week==week,]
  week$prediction <- week$espn.dk.points
  week$salary <- week$dk.salary
  sol <- solve(week, verbose = F)
  predicted <- sum(week[which(sol$solution==1),]$espn.dk.points)
  actual <- sum(week[which(sol$solution==1),]$dk.points)
  paste("predicted:", predicted, "actual:", actual, "diff", actual - predicted)
}
