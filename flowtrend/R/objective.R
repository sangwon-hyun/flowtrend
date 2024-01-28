# Generated from _main.Rmd: do not edit by hand

#' Evaluating the penalized data log-likelihood on all data \code{ylist} given parameters \code{mu}, \code{prob}, and \code{sigma}.
#'
#' @param mu
#' @param prob
#' @param prob_link
#' @param sigma
#' @param ylist
#' @param Dl
#' @param l
#' @param lambda
#' @param l_prob
#' @param Dl_prob
#' @param lambda_prob
#' @param alpha
#' @param beta
#' @param denslist_by_clust
#' @param countslist
#' @param unpenalized if TRUE, return the unpenalized out-of-sample fit.
#'
#' @return
#' @export
#'
#' @examples
objective <- function(mu, prob, prob_link = NULL, sigma,
                      ## TT, N, dimdat, numclust,
                      ylist,
                      Dlp1, l = NULL,
                      lambda = 0,
                      l_prob = NULL,
                      Dlp1_prob = NULL,
                      lambda_prob = 0,
                      alpha = NULL, beta = NULL,
                      denslist_by_clust = NULL,
                      countslist = NULL,
                      unpenalized = FALSE){

  ## Set some important variables
  TT = dim(mu)[1]
  numclust = dim(mu)[3]
  if(is.null(countslist)){
    ntlist = sapply(ylist, nrow)
  } else {
    ntlist = sapply(countslist, sum)
  }
  N = sum(ntlist)
  dimdat = ncol(ylist[[1]])

  ## Calculate the log likelihood
  loglik = sapply(1:TT, function(tt){
    if(is.null(denslist_by_clust)){
      return(loglik_tt(ylist, tt, mu, sigma, prob, countslist, numclust = numclust, dimdat = dimdat))
    } else {
      ## TODO: This function doesn't exist yet, but might need to, since.. speed!
      return(loglik_tt_precalculate(ylist, tt, denslist_by_clust, prob, countslist, numclust))
    }
  })

  if(unpenalized){
    obj =  -1/N * sum(unlist(loglik)) 
    return(obj)
  } else {

    ## Return penalized likelihood
    mu.splt <- asplit(mu, MARGIN = 3)
    diff_mu <- sum(unlist(lapply(mu.splt, FUN = function(m) sum(abs(Dlp1 %*% m)))))
    diff_prob <- sum(abs(Dlp1_prob %*% prob_link))
    obj =  -1/N * sum(unlist(loglik)) + lambda * diff_mu + lambda_prob * diff_prob
    return(obj)
  }
}

