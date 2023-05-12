# Generated from _main.Rmd: do not edit by hand

#' LA-ADMM requires an convergence check for the outer layer.
#' 
#' @param objectives Objectives of the outer layer.
#'
#' @return TRUE if relative improvement is smaller than 1E-5 in the last four
#'   outer iterations' objective.
#' @export
outer_converge <- function(objectives){
  consec = 4
  if(length(objectives) < consec){
    return(FALSE)
  } else {
    mytail = utils::tail(objectives, consec)
    rel_diffs = mytail[1:(consec-1)]/mytail[2:consec]
    return(all(abs(rel_diffs) - 1 < 1E-5))
  }
}

