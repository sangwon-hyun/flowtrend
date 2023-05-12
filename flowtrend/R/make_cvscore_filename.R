# Generated from _main.Rmd: do not edit by hand

#' Create file name (a string) for cross-validation results.
#' @param iprob
#' @param imu
#' @param ifold
#' @param irestart
#' 
#' @export
make_cvscore_filename <- function(iprob, imu, ifold, irestart){
  filename = paste0(iprob, "-", imu, "-", ifold, "-", irestart, "-cvscore.Rdata")
  return(filename)
}


#' Create file name (a string) for re-estimated models for the lambda values
#' indexed by \code{iprob} and \code{imu}.
#' @param iprob
#' @param imu
#' @param irestart
#' 
#' @export
make_refit_filename <- function(iprob, imu, irestart){
  filename = paste0(iprob, "-", imu, "-", irestart, "-fit.Rdata")
  return(filename)
}
