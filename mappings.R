
flip <- function(a){
  b <- c(names(a))
  names(b) <- a
  b
}

availableToHistoryPosition = c(
  "DST"="Def"
)

historyToAvailablePosition <- flip(availableToHistoryPosition)

availableToHistoryTeam <- c(
  "ARI"="ari",
  "ATL"="atl",
  "BAL"="bal",
  "BUF"="buf",
  "CAR"="car",
  "CHI"="chi",
  "CIN"="cin",
  "CLE"="cle",
  "DAL"="dal",
  "DEN"="den",
  "DET"="det",
  "GB"="gnb",
  "HOU"="hou",
  "IND"="ind",
  "JAX"="jac",
  "KC"="kan",
  "LAC"="lac",
  "LAR"="lar",
  "MIA"="mia",
  "MIN"="min",
  "NE"="ne",
  "NO"="nor",
  "NYJ"="nyj",
  "NYG"="nyg",
  "OAK"="oak",
  "PIT"="pit",
  "PHI"="phi",
  "SEA"="sea",
  "SF"="sfo",
  "TB"="tam",
  "TEN"="ten",
  "WAS"="was"
)

historyToAvailableTeam <- flip(availableToHistoryTeam)

historyToAvailableName <- c(
  "Arizona"="Cardinals",
  "Atlanta"="Falcons",
  "Baltimore"="Ravens",
  "Buffalo"="Bills",
  "Carolina"="Panthers",
  "Chicago"="Bears",
  "Cincinnati"="Bengals",
  "Cleveland"="Cleveland",
  "Dallas"="Cowboys",
  "Denver"="Broncos",
  "Detroit"="Lions",
  "Green Bay"="Packers",
  "Houston"="Texans",
  "Indianapolis"="Colts",
  "Jacksonville"="Jaguars",
  "Kansas City"="Chiefs",
  "LA Chargers"="Chargers",
  "LA Rams"="Rams",
  "Miami"="Dolphins",
  "Minnesota"="Vikings",
  "New England"="Patriots",
  "New Orleans"="Saints",
  "New York J"="Jets",
  "New York G"="Giants",
  "Oakland"="Raiders",
  "Pittsburgh"="Steelers",
  "Philadelphia"="Eagles",
  "Seattle"="Seahawks",
  "San Francisco"="49ers",
  "Tampa Bay"="Tampa Bay",
  "Tennessee"="Titans",
  "Washington"="Redskins"
)

availableToHistoryName = flip(historyToAvailableName)

mapOrDefault <- function(x, mappings, default=NA) {
  xMatch <- trimws(as.character(x))
  if (xMatch %in% names(mappings)){
    as.character(mappings[xMatch])
  } else {
    as.character(default)
  }
}

softMatch <- function(x, possible){
  if (x %in% possible) {
    x
  } else {
    alt <- gsub(" [I|V]+$", "", x)
    alt <- gsub(" $", "", alt)
    if (alt %in% possible) alt else x 
  }
}


