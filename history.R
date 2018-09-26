library('RCurl')
library('xml2')
source("mappings.R")
source("utils.R")

LATEST_HISTORY_FILE = "history.csv"
HISTORY_BASE_URL <- "http://rotoguru1.com/cgi-bin/fyday.pl?game=dk&scsv=1&week=WEEK&year=YEAR"

getLatestHistory <- function(f=LATEST_HISTORY_FILE){
  as.data.frame(read.csv(LATEST_HISTORY_FILE))  
}

saveHistory <- function(h, f=LATEST_HISTORY_FILE){
  write.csv(h, f)
}

downloadHistoric <- function(url){
  out <- getURL(url, .opts = list(ssl.verifypeer = FALSE) )
  html <- read_html(out)
  tableText <- xml_text(xml_find_all(html,".//pre"))
  read.csv(text=tableText,sep=";")
}

updateHistory <- function(h = data.frame()){
  maxNumWeeks <- 17
  minNumYear <- 2014
  out <- rbind(h)
  if (nrow(h) == 0) {
    maxYear <- minNumYear - 1
    maxWeek <- maxNumWeeks
  } else {
    maxYear <- max(out$year)
    maxWeek <- max(out[out$year == maxYear,]$week)
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
    if (nrow(out) == 0){
      out <- curHistory
    } else{
      out <- rbind(out, curHistory)
    }
  }
  return(out)
}

toDisplayName <- function(name){
  trimws(paste(rev(unlist(strsplit(name, ", "))), collapse = " "))
}

enrichHistory <- function(h){
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
  
  out$dk.points.log <- sapply(out$dk.points, function(x){log(max(0.0001, x))})

  out$key <- paste(out$year, out$displayName)
  gameCounts <- as.data.frame(table(out$key))
  colnames(gameCounts) <- c("key","games")

  out <- merge(out, gameCounts, by="key")
  out$key <- NULL
  
  return(out)
}

