# Generated from _main.Rmd: do not edit by hand

#' @param mu Cluster means.
#' @param prob Cluster probabilities.
#' @param prob Cluster variances.
#' @param ylist Data.
#' @param tt Time point.
loglik_tt <- function(ylist, tt, mu, sigma, prob, dimdat = NULL, countslist = NULL, numclust){

  ## One particle's log likelihood (weighted density)
  weighted.densities = sapply(1:numclust, function(iclust){
    if(dimdat == 1){
      return(prob[tt,iclust] * dnorm(ylist[[tt]], mu[tt,,iclust], sqrt(sigma[iclust,,])))
    }
    if(dimdat > 1){
    return(prob[tt,iclust] * dmvnorm_arma_fast(ylist[[tt]], mu[tt,,iclust], as.matrix(sigma[iclust,,]), FALSE))
    }
  })
  nt = nrow(ylist[[tt]])
  counts = (if(!is.null(countslist)) countslist[[tt]] else rep(1, nt))

  sum_wt_dens = rowSums(weighted.densities)
  sum_wt_dens = sum_wt_dens %>% pmax(1E-100)

  return(sum(log(sum_wt_dens) * counts))
}
