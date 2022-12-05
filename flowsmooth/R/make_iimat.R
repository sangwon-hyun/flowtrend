# Generated from _main.Rmd: do not edit by hand

#' Indices for the cross validation jobs.
#'
#' The resulting iimat looks like this:
#'
#' ind    iprob  imu ifold irestart
#'  55      6     1     2    1 
#'  56      7     1     2    1
#'  57      1     2     2    1
#'  58      2     2     2    1
#'  59      3     2     2    1
#'  60      4     2     2    1
#' @param cv_gridsize CV grid size.
#' @param nfold Number of CV folds.
#' @param nrestart Number of random restarts of EM algorithm.
#'
#' @return Integer matrix.
#'
#' @export
make_iimat <- function(cv_gridsize, nfold, nrestart){
  iimat = expand.grid(iprob = 1:cv_gridsize,
                      imu = 1:cv_gridsize,
                      ifold = 1:nfold,
                      irestart = 1:nrestart)
  iimat = cbind(ind = as.numeric(rownames(iimat)), iimat)
  return(iimat)
}

#' 2d indices for the cross validation jobs.
#'
#' The resulting iimat looks like this:
#' (#, iprob, imu, irestart)
#' 1, 1, 1, 1
#' 2, 1, 2, 1
#' 3, 1, 3, 1
#'
#' @inheritParams make_iimat
#'
#' @return Integer matrix.
#'
#' @export
make_iimat_small <- function(cv_gridsize, nrestart){
  iimat = expand.grid(iprob = 1:cv_gridsize,
                      imu = 1:cv_gridsize,
                      irestart = 1:nrestart)
  iimat = cbind(ind = as.numeric(rownames(iimat)), iimat)
  return(iimat)
}

