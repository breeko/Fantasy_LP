toDisplayName <- function(name){
  trimws(paste(rev(unlist(strsplit(as.character(name), ", "))), collapse = " "))
}

mapValues <- function(l, dict){
  slice <- l %in% names(dict)
  l[slice] <- dict[l[slice]]
  return(l)
}

convertToDate <- function(year, week, startMonth=9, startDay=1){
  as.Date(paste(year,startMonth,startDay,sep="-")) + as.numeric(week) * 7
}

writeToFile <- function(x, f="summary.txt"){
  oldMaxPrint <- getOption("max.print")
  sink(f)
  options(max.print=10000)
  print(x)
  options(max.print=oldMaxPrint)
  sink()
  f
}

gsubs <- function(matches, replaces, x){
  if (length(matches) != length(replaces)) stop("Matches must equal replaces in length")
  out <- x
  for (idx in 1:length(matches)){
    out <- gsub(matches[idx], replaces[idx], out)
  }
  return(out)
}
