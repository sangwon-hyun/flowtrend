# Generated from _main.Rmd: do not edit by hand

#' Estimate maximum lambda values numerically.  First starts with a large
#' initial value \code{max_lambda_mean} and \code{max_lambda_prob}, and runs
#' the EM algorithm on decreasing set of values (sequentially halved). This
#' stops once you see non-flat probabilities or means, and returns the *smallest*
#' regularization (lambda) value pair that gives full sparsity.
#'
#' Note that the \code{zero_stabilize=TRUE} option is used in
#' \code{flowsmooth()}, which basically means the EM algorithm runs only until
#' the zero pattern stabilizes.
#'
#' @param ylist List of responses.
#' @param numclust Number of clusters.
#' @param max_lambda_mean Defaults to 4000.
#' @param max_lambda_prob Defaults to 1000.
#' @param iimax Maximum value of x for 2^{-x} factors to try.
#' @param ... Other arguments to \code{flowsmooth_once()}.
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

  ## Get range of regularization parameters.
  facs = sapply(1:iimax, function(ii) 2^(-ii+1)) ## DECREASING order
  print("running the models once")
  for(ii in 1:iimax){

    ## print_progress(ii, iimax, "regularization values", fill = TRUE)
    cat("###############################################################", fill=TRUE)
    cat("#### lambda_prob = ", max_lambda_prob * facs[ii],
        " and lambda_beta = ", max_lambda_mean * facs[ii], "being tested.  ", fill=TRUE)
    cat("###############################################################", fill=TRUE)

    res = flowsmooth_once(ylist = ylist,
                       countslist = countslist,
                       numclust = numclust,
                       lambda_prob = max_lambda_prob * facs[ii],
                       lambda_mean = max_lambda_mean * facs[ii],
                       verbose = verbose,
                       zero_stabilize = TRUE,
                       ...)

    ## TODO: CHECK FLATNESS INSTEAD OF ZERONESS

    ## Check zero-ness
    toler = 0
    sum_nonzero_prob = sum(res$alpha[,-1] > toler)
    sum_nonzero_beta = sum(unlist(lapply(res$beta, function(cf){ sum(cf[-1,] > toler) })))


    ## If there are *any* nonzero values, do one of the following
    if(sum_nonzero_alpha + sum_nonzero_beta != 0){


      ## If there are *any* nonzero values at the first iter, prompt a restart
      ## with higher initial lambda values.
      if(ii==1){
        stop(paste0("Max lambdas: ", max_lambda_mean, " and ", max_lambda_prob,
                    " were too small as maximum reg. values. Go up and try again!!"))

      ## If there are *any* nonzero values, return the immediately preceding
      ## lambda values -- these were the smallest values we had found that gives
      ## full sparsity.
      } else {
        ## Check one more time whether the model was actually zero, by fully running it;
        res = flowsmooth_once(ylist = ylist,
                              countslist = countslist,
                              numclust = numclust,
                              lambda_prob = max_lambda_prob * facs[ii],
                              lambda      = max_lambda_mean * facs[ii],
                              zero_stabilize = FALSE,
                              ...)
        toler = 0
        sum_nonzero_alpha = sum(res$alpha[,-1] > toler)
        sum_nonzero_beta = sum(unlist(lapply(res$beta, function(cf){ sum(cf[-1,] > toler) })))

        ## If there are *any* nonzero values, do one of the following
        if(sum_nonzero_alpha + sum_nonzero_beta != 0){
          return(list(beta = max_lambda_mean * facs[ii-1],
                      alpha = max_lambda_prob *facs[ii-1]))
        }
        ## Otherwise, just proceed to the next iteration.
      }
    }
    cat(fill=TRUE)
  }
}
