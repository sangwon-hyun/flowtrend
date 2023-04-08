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
        " and lambda = ", max_lambda_mean * facs[ii], "being tested.  ", fill=TRUE)
    cat("###############################################################", fill=TRUE)

    res = flowsmooth_once(ylist = ylist,
                          countslist = countslist,
                          numclust = numclust,
                          lambda_prob = max_lambda_prob * facs[ii],
                          lambda = max_lambda_mean * facs[ii],
                          verbose = verbose,
                          ...)

    ## TODO: CHECK FLATNESS INSTEAD OF ZERONESS
    ## In each dimension, the data should only vary by a relatively small amount (say 1/100)
    browser()
    idim = 1
    toler_by_dim = sapply(1:res$dimdat, function(idim){
      datrange = ylist %>% sapply(FUN = function(y) y %>% .[,idim] %>% range()) %>% range()
      toler = (datrange[2] - datrange[1]) / (100 * length(ylist))
    })
    mean_is_flat = sapply(1:res$dimdat, FUN = function(idim){
      all(abs(diff(res$mn[,idim,])) < toler_by_dim)
    })

    toler_prob = 0.01 / length(ylist)
    prob_is_flat =  all(abs(diff(res$prob)) < toler_prob)
    if(all(mean_is_flat) & prob_is_flat) ## I think this is it?

    abs_range / 100
    plot_1d(ylist, res)
    plot_prob(res) + ylim(c(0,1))
    names(res)
    res$lambda
    res$lmbda_prob
    res$prob[,1] %>% plot()
    res$objectives %>% plot(type = 'o', cex=.5)


    ## End of the flatness

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
                              ...)

        ## Check if both curves are basically flat
        if(FALSE){
        toler = 0
        sum_nonzero_alpha = sum(res$alpha[,-1] > toler)
        sum_nonzero_beta = sum(unlist(lapply(res$beta, function(cf){ sum(cf[-1,] > toler) })))
        }
        res$mean
        res$prob

        ## If there are *any* nonzero values, do one of the following
        if(sum_nonzero_alpha + sum_nonzero_beta != 0){
          return(list(mean = max_lambda_mean * facs[ii-1],
                      prob = max_lambda_prob *facs[ii-1]))
        }
        ## Otherwise, just proceed to the next iteration.
      }
    }
    cat(fill=TRUE)
  }
}
