source("mappings.R")

STATUS_URL_BASE = "https://api.draftkings.com/draftgroups/v1/draftgroups/WEEKS_ID/draftables?format=json"
AVAILABLE_URL_BASE <- "https://www.draftkings.com/lineup/getavailableplayerscsv?contestTypeId=21&draftGroupId=WEEKS_ID"

getLatestAvailable <- function(weeksId){
  # Contains the status of players (e.g. out, questionable)
  statusUrl <- gsub("WEEKS_ID", weeksId, STATUS_URL_BASE)
  # Contains the available linup
  availableUrl <- gsub("WEEKS_ID", weeksId, AVAILABLE_URL_BASE)
  
  availablePlayers <- read.csv(availableUrl)
  colnames(availablePlayers) <- tolower(colnames(availablePlayers))
  
  # Look up player status
  playerStatus <- jsonlite::read_json(statusUrl)
  playerStatus <- lapply(playerStatus$draftables, function(r){r[c("displayName","position","teamAbbreviation","status")]})
  playerStatus <- do.call(rbind, lapply(playerStatus, as.data.frame))
  colnames(playerStatus) <- c("name","position","teamabbrev","status")
  playerStatus$key <- paste(playerStatus$name, playerStatus$position, playerStatus$teamabbrev, sep="-")
  playerStatus <- unique(playerStatus)
  
  availablePlayers$key <- paste(availablePlayers$name, availablePlayers$position, availablePlayers$teamabbrev, sep="-")
  
  # Add player status to available
  availablePlayers <- merge(availablePlayers, playerStatus[c("key","status")], by="key", ALL=T)
  availablePlayers$key <- NULL
  
  # Remove questionable players
  availablePlayers <- availablePlayers[c(availablePlayers$status == "None"),]
  return(as.data.frame(availablePlayers))
}

availableToHistory <- function(a, week, year, history=NULL){
  out <- c()
  out$displayName <- unlist(lapply(a$name, function(x){mapOrDefault(x, availableToHistoryName, x)}))
  if (!is.null(history$displayName)){
    out$displayName <- unlist(lapply(out$displayName, function(x){softMatch(x, unique(history$displayName))}))
  }
  out$week <- as.factor(week)
  out$year <- as.factor(year)
  out$pos <- unlist(lapply(a$position, function(x){mapOrDefault(x,availableToHistoryPosition, x)}))
  out$team <- unlist(lapply(a$teamabbrev, function(x){mapOrDefault(x, availableToHistoryTeam)}))
  out$oppt <- unlist(mapply(getOpp, a$game.info, a$team))
  out$oppt <- unlist(lapply(out$oppt, function(x){mapOrDefault(x, availableToHistoryTeam)}))
  out$h.a <- unlist(mapply(getHomeAway, a$game.info, a$team))
  out$teamYear <- unlist(lapply(out$team, function(x){paste(x, year, sep="-")}))
  out$opptYear <- unlist(lapply(out$team, function(x){paste(x, year, sep="-")}))
  out$salary <- a$salary
  return(as.data.frame(out))
}

getOpp <- function(info, otherTeam){
  teams <- strsplit(gsub(" .*", "", info),"@")[[1]]
  setdiff(teams, otherTeam)
}

getHomeAway <- function(info, team){
  # Returns h or a to represent home or away from string [TEAM] @ [TEAM], defaults to h
  # e.g. f("DEN @ NJG", "DEN") => "h"
  home <- strsplit(gsub(" .*", "", info),"@")[[1]][2]
  if (!is.na(home)){
    if (home == team) return("h") else return("a")
  }
  return("h")
}
