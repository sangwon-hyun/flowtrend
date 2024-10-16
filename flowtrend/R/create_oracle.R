# Generated from _main.Rmd: do not edit by hand

#' Reformatting |datobj| to create a flowtrend-like object that contains
#' "oracle" information of the model that
#' generates the simulated pseudo-real data.
#'
#' @param datobj A data object.
#'
#' @return A flowtrend-like object containing mn, sigma, and prob.
#'
#' @export
create_oracle <- function(datobj){

  TT = nrow(datobj$mns)
  dimdat = 1
  numclust = ncol(datobj$mns)

  ## Make a fake object in a similar format as a |flowtrend| object
  fake_obj = list()
  fake_obj$mn = array(NA, dim = c(TT, dimdat,numclust))
  fake_obj$mn[,1,] = datobj$mns
  fake_obj$prob = datobj$prob
  fake_obj$sigma = array(NA, dim = c(numclust, dimdat, dimdat))
  fake_obj$sigma[1,1,1] = datobj$sd1^2
  fake_obj$sigma[2,1,1] = datobj$sd2^2
  fake_obj$numclust = numclust
  return(fake_obj)
}
