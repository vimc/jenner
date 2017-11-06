## Rolling mean calculations.  This relies on the data being a vector
## that represents a row-wise matrix with 'n' rows.  The average is
## computed on each group with a width of 'w'.  The *start* of the
## matrix is padded with missing values, which are then excluded from
## the sum.  This does produce biased estimates through these regions.
roll_mean_by <- function(x, w, n) {
  c(roll_fun(matrix(x, n), w, "mean"))
}

roll_sum_by <- function(x, w, n) {
  c(roll_fun(matrix(x, n), w, "sum"))
}

roll_fun <- function(x, w, fun = "mean") {
  if (w == Inf) {
    roll_fun_total(x, fun)
  } else {
    roll_fun_window(x, w, fun)
  }
}

roll_fun_window <- function(x, w, fun = "mean") {
  if (w %% 2 != 1) {
    stop("Expected odd 'w'")
  }
  k <- (w - 1) %/% 2
  if (is.matrix(x)) {
    pad <- matrix(NA_real_, k, ncol(x))
    xx <- rbind(pad, x, pad, deparse.level = 0)
  } else {
    pad <- rep(NA_real_, k)
    xx <- c(pad, x, pad)
  }

  ret <- switch(fun,
                mean = RcppRoll::roll_mean(xx, w, na.rm = TRUE),
                sum = RcppRoll::roll_sum(xx, w, na.rm = TRUE),
                stop("Unimplemented function ", fun))

  ret[is.nan(ret)] <- NA_real_
  ret
}
roll_fun_total <- function(x, fun = "mean") {
  colFun <- switch(fun,
                   mean = colMeans,
                   sum = colSums,
                   stop("Unimplemented function ", fun))
  xx <- colFun(x, na.rm = TRUE)
  xx[is.nan(xx)] <- NA_real_
  matrix(xx, nrow(x), ncol(x), byrow = TRUE)
}
