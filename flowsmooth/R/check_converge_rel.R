# Generated from _main.Rmd: do not edit by hand

#' Checks numerical improvement in objective value. Returns TRUE if |old-new|/|old| is smaller than tol.
#'
#' @param old Objective value from previous iteration.
#' @param new Objective value from current iteration.
#' @param tol Numerical tolerance.
check_converge_rel <- function(old, new, tol=1E-6){
  return(abs((old-new)/old) < tol )
}
