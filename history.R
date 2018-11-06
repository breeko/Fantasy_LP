library('RCurl')
library('xml2')
source("mappings.R")
source("utils.R")

LATEST_HISTORY_FILE = "history.csv"
HISTORY_BASE_URL <- "http://rotoguru1.com/cgi-bin/fyday.pl?game=dk&scsv=1&week=WEEK&year=YEAR"

getLatestHistory <- function(f=LATEST_HISTORY_FILE){
  # Returns latest saved history from provided file or default
  as.data.frame(read.csv(LATEST_HISTORY_FILE))  
}

saveHistory <- function(h, f=LATEST_HISTORY_FILE){
  # Writes data frame to provided file or default history file
  write.csv(h, f, row.names = F)
}

downloadHistoric <- function(url){
  # Downloads single historic table
  out <- getURL(url, .opts = list(ssl.verifypeer = FALSE) )
  html <- read_html(out)
  tableText <- xml_text(xml_find_all(html,".//pre"))
  read.csv(text=tableText,sep=";")
}

updateHistory <- function(h = data.frame(), save=F){
  # Updates history or generates a new history from 2014. Optionally save it to default file
  maxNumWeeks <- 17
  minNumYear <- 2014
  out <- rbind(h)
  if (nrow(h) == 0) {
    maxYear <- minNumYear - 1
    maxWeek <- maxNumWeeks
  } else {
    maxYear <- max(out$Year)
    maxWeek <- max(out[out$Year == maxYear,]$Week)
  }
  curYear <- maxYear
  curWeek <- maxWeek
  while(T){
    curWeek <- max(1, (curWeek + 1) %% (maxNumWeeks + 1))
    curYear <- if (curWeek == 1) curYear + 1 else curYear
    curUrl <- gsub("WEEK", curWeek, gsub("YEAR", curYear, HISTORY_BASE_URL))
    print(curUrl)
    curHistory <- downloadHistoric(curUrl)
    if (nrow(curHistory) == 0) break
    out <- rbind(out, curHistory)
  }
  if (save) saveHistory(out)
  return(out)
}

toDisplayName <- function(name){
  # Converts Smith, Bob -> Bob Smith
  trimws(paste(rev(unlist(strsplit(name, ", "))), collapse = " "))
}

enrichHistory <- function(h){
  # Adds information to history
  out <- as.data.frame(rbind(h))
  colnames(out) <- tolower(colnames(out))
  
  out$displayName <- apply(out["name"], 1, toDisplayName)
  
  # Baltimore -> Ravens
  out$displayName <- mapValues(out$displayName, historyToAvailableTeam)
  
  out$year <- as.factor(out$year)
  out$week <- as.factor(out$week)

  # Converting year and week to date
  out$date <- as.Date(mapply(convertToDate, as.character(out$year), as.character(out$week)), origin = "1970-01-01")
  
  # Adding factor for teams given year
  out$teamYear <- paste(out$team, out$year, sep="-")
  out$opptYear <- paste(out$oppt, out$year, sep="-")
  
  # Adding log of points
  out$dk.points.log <- sapply(out$dk.points, function(x){log(max(0.0001, x))})
  
  # Adding game counts
  out$key <- paste(out$year, out$displayName)
  gameCounts <- as.data.frame(table(out$key))
  colnames(gameCounts) <- c("key","games")
  out <- merge(out, gameCounts, by="key")
  out$key <- NULL
  
  # Adding annual point median by player by year
  gb<-aggregate(out[, "dk.points"], list(out$name, out$year), median)
  names(gb) <- c("name", "year", "median")
  out<-merge(out, gb, by = c("name", "year"))

  # Adding annual point median by player by year
  gb<-aggregate(out[, "dk.points"], list(out$name, out$year), mean)
  names(gb) <- c("name", "year", "mean")
  out<-merge(out, gb, by = c("name", "year"))
  
  return(out)
}

