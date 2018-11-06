library(lpSolve)

solve <- function(a, budget=50000, verbose=T) {
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
      a$pos %in% c("WR", "RB"),
      a$pos %in% c("TE", "RB"),
      a$pos %in% c("TE", "WR"),
      a$salary
    ), nrow=10, byrow=TRUE
  )
  # QB: 1 RB: 2, WR: 3, TE: 1, Def: 1 FLEX: 1
  f.dir <- rep("<=", 10)
  f.rhs <- c(1, 3, 4, 2, 1, 7, 6, 5, 5, budget)
  sol <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = T)
  if (verbose) print(a[which(sol$solution==1),])
  return(sol)
}
