# Generated from _main.Rmd: do not edit by hand

#' Do a linear interpolation of the cluster means.
#'
#' @param x Training times.
#' @param tt Prediction time.
#' @param iclust Cluster number.
#' @param prob length(x) by numclust array or matrix.
#'
#' @return One probability.
interpolate_prob <- function(x, tt, iclust, prob){

  ## Basic checks
  numdat = dim(prob)[1]
  numclust = dim(prob)[2]
  stopifnot(length(x) == numdat)
  stopifnot(iclust <= numclust)
  if(tt %in% x) return(prob[which(x == tt),iclust,drop=TRUE])

  ## Set up for linear interpolation
  floor_t <- max(x[which(x <= tt)])
  ceiling_t <- min(x[which(x >= tt)])
  floor_t_ind <- which(x == floor_t)
  ceiling_t_ind <- which(x == ceiling_t)

  ## Do the linear interpolation
  prob_t <-
    prob[ceiling_t_ind,iclust,drop=TRUE]*(tt - floor_t)/(ceiling_t - floor_t) +
    prob[floor_t_ind,iclust,drop=TRUE]*(ceiling_t - tt)/(ceiling_t - floor_t) # linear interpolation between floor_t and ceiling_t

  ## Basic checks
  stopifnot(length(prob_t) == 1)
  stopifnot(0 <= prob_t & prob_t <= 1)

  return(prob_t)
}
