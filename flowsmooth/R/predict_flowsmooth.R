# Generated from _main.Rmd: do not edit by hand

#' Prediction: Given new timepoints in the original time interval,generate a set
#' of means and probs (and return the same Sigma).
#'
#' @param obj Object returned from covariate EM flowsmooth().
#' @param newtimes New times at which to make predictions.
#'
#' @return List containing mean, prob, and sigma, and x.
#'
#' @export
#'
predict_flowsmooth <- function(obj, newtimes = NULL){

  ## Check the dimensions
  newx <- newtimes
  if(is.null(newtimes)){ newx = obj$x }

  ## Check if the new times are within the time range of the original data 
  stopifnot(all(sapply(newx, FUN = function(t) t >= min(obj$x) & t <= max(obj$x))))

  ## Setup some things
  x <- obj$x
  TT_new = length(newx)
  numclust = obj$numclust
  dimdat = obj$dimdat

  ## Predict the means (manually).
  newmn_array = array(NA, dim = c(TT_new, dimdat, numclust))
  for(iclust in 1:numclust){
    newmn_oneclust <- lapply(newx, function(tt){
      interpolate_mn(x, tt, iclust, obj$mn)
    }) %>% do.call(rbind, . )
    newmn_array[,,iclust] = newmn_oneclust
  }

  ## Predict the probs.
  newprob = array(NA, dim = c(TT_new, numclust))
  for(iclust in 1:numclust){
    newprob_oneclust <- lapply(newx, function(tt){
      interpolate_prob(x, tt, iclust, obj$prob)
    }) %>% do.call(c, .)
    newprob[,iclust] = newprob_oneclust
  }

  ## Basic checks
  stopifnot(all(dim(newprob) == c(TT_new,numclust)))
  stopifnot(all(newprob >= 0))
  stopifnot(all(newprob <= 1))

  ## Return the predictions
  return(list(mn = newmn_array,
              prob = newprob,
              sigma = obj$sigma,
              x = newx))
}
