library(lpSolve)

solve <- function(a, budget=50000) {
  # Must have prediction, pos and salary
  requiredFields <- c("prediction", "pos", "salary")
  if (!all(requiredFields %in% names(a))) {
    stop(paste("Invalid input. Must have fields:", paste(requiredFields, collapse = ", ")))
  }
  f.obj <- a$prediction
  f.con <- matrix(
    c(a$pos == "QB",
      a$pos == "RB",
      a$pos == "WR",
      a$pos == "TE",
      a$pos == "Def",
      a$pos %in% c("TE", "WR", "RB"),
      a$salary
    ), nrow=7, byrow=TRUE
  )
  
  f.dir <- rep("<=", 7)
  f.rhs <- c(1, 3, 4, 2, 1, 7, budget)
  sol <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = T)
  
  print(a[which(sol$solution==1),])
  return(sol)
}
