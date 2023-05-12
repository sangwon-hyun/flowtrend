# Generated from _main.Rmd: do not edit by hand

#' Do a linear interpolation of the cluster means.
#'
#' @param x Training times.
#' @param tt Prediction time.
#' @param iclust Cluster number.
#' @param mn length(x) by dimdat by numclust matrix.
#'
#' @return A dimdat-length vector.
interpolate_mn <- function(x, tt, iclust, mn){

  ## Basic checks
  stopifnot(length(x) == dim(mn)[1])
  stopifnot(iclust <= dim(mn)[3])
  if(tt %in% x) return(mn[which(x==tt),,iclust,drop=TRUE])

  ## Set up for linear interpolation
  floor_t <- max(x[which(x <= tt)])
  ceiling_t <- min(x[which(x >= tt)])
  floor_t_ind <- which(x == floor_t)
  ceiling_t_ind <- which(x == ceiling_t)

  ## Do the linear interpolation
  mn_t <-
    mn[ceiling_t_ind,,iclust,drop=TRUE]*(tt - floor_t)/(ceiling_t - floor_t) +
    mn[floor_t_ind,,iclust,drop=TRUE]*(ceiling_t - tt)/(ceiling_t - floor_t) 

  ## Basic checks
  stopifnot(length(mn_t) == dim(mn)[2])

  return(mn_t)
}
