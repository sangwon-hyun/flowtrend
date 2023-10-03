# Generated from _main.Rmd: do not edit by hand

#' Estimate maximum lambda values numerically.  First starts with a large
#' initial value \code{max_lambda_mean} and \code{max_lambda_prob}, and runs
#' the EM algorithm on decreasing set of values (sequentially halved). This
#' stops once you see non-flat probabilities or means, and returns the *smallest*
#' regularization (lambda) value pair that gives full sparsity.
#'
#' Note that the \code{zero_stabilize=TRUE} option is used in
#' \code{flowtrend()}, which basically means the EM algorithm runs only until
#' the zero pattern stabilizes.
#'
#' @param ylist List of responses.
#' @param numclust Number of clusters.
#' @param max_lambda_mean Defaults to 4000.
#' @param max_lambda_prob Defaults to 1000.
#' @param iimax Maximum value of x for 2^{-x} factors to try.
#' @param ... Other arguments to \code{flowtrend_once()}.
#' 
#' @return list containing the two maximum values to use.
#' 
#' @export
calc_max_lambda <- function(ylist, countslist = NULL, numclust, 
                            max_lambda_mean = 4000,
                            max_lambda_prob = 1000,
                            verbose = FALSE,
                            iimax = 16,
                            ...){

  ## Basic setup
  dimdat = ncol(ylist[[1]])
  toler_by_dim = sapply(1:dimdat, function(idim){
    datrange = ylist %>% sapply(FUN = function(y) y %>% .[,idim] %>% range()) %>% range()
    toler = (datrange[2] - datrange[1]) / (100 * length(ylist))
  })
  ## This is arbitrary... I found that with T=100, nt=100 examples that it's reasonable.
  toler_prob = min(0.1 / length(ylist), 0.01)

  ## Get range of regularization parameters.
  facs = sapply(1:iimax, function(ii) 2^(-ii+1)) ## DECREASING order
  print("running the models once")
  for(ii in 1:iimax){

    cat("###############################################################", fill=TRUE)
    cat("#### lambda_prob = ", max_lambda_prob * facs[ii],
        " and lambda = ", max_lambda_mean * facs[ii], "being tested.  ", fill=TRUE)
    cat("###############################################################", fill=TRUE)

    res = flowtrend_once(ylist = ylist,
                          countslist = countslist,
                          numclust = numclust,
                          lambda_prob = max_lambda_prob * facs[ii],
                          lambda = max_lambda_mean * facs[ii],
                          verbose = verbose, ...)

    ## In each dimension, the data should only vary by a relatively small amount (say 1/100)
    mean_is_flat = sapply(1:dimdat, FUN = function(idim){
      all(abs(diff(res$mn[,idim,])) < toler_by_dim[idim])  })
    prob_is_flat = all(abs(diff(res$prob)) < toler_prob)
    all_are_flat = (all(mean_is_flat) & prob_is_flat)

    ## ## Temporary
    ## par(mfrow=c(1,3))
    ## matplot(res$prob, ylim=c(0,1))
    ## diff(diff(res$prob)) %>% matplot()
    ## diff(res$prob) %>% matplot()
    ## cumsum(diff(diff(res$prob))[,1]) %>% lines(col='orange')
    ## ## End of temporary

    if(!all_are_flat){

      ## If there are *any* nonzero values at the first iter, prompt a restart
      ## with higher initial lambda values.
      if(ii == 1){
        stop(paste0("Max lambdas: ", max_lambda_mean, " and ", max_lambda_prob,
                    " were too small as maximum reg. values. Go up and try again!!"))

      ## If there are *any* nonzero values, return the immediately preceding
      ## lambda values -- these were the smallest values we had found that gives
      ## full sparsity.
      } else {
        ## Check one more time whether the model was actually zero, by fully running it;
        res = flowtrend_once(ylist = ylist,
                              countslist = countslist,
                              numclust = numclust,
                              lambda_prob = max_lambda_prob * facs[ii],
                              lambda      = max_lambda_mean * facs[ii],
                              ...)

        ## Check if both curves are basically flat
        mean_is_flat = sapply(1:res$dimdat, FUN = function(idim){
          all(abs(diff(res$mn[,idim,])) < toler_by_dim[idim]) })
        prob_is_flat =  all(abs(diff(res$prob)) < toler_prob)
        all_are_flat = (all(mean_is_flat) & prob_is_flat)

        ## If there are *any* nonzero values, stop.
        ## (Otherwise, just proceed to try a smaller set of lambdas.)
        if(!all_are_flat){
          return(list(mean = max_lambda_mean * facs[ii-1],
                      prob = max_lambda_prob * facs[ii-1]))
        }
      }
    }
    cat(fill=TRUE)
  }
}
