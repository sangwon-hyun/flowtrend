# Generated from _main.Rmd: do not edit by hand

#' M step for cluster covariance (sigma).
#'
#' @param resp Responsibility.
#' @param ylist Data.
#' @param mn Means
#' @param numclust Number of clusters.
#'
#' @return (K x d x d) array containing K (d x d) covariance matrices.
#' @export
#'
#' @examples
Mstep_sigma <- function(resp, ylist, mn, numclust){

  ## Find some sizes
  TT = length(ylist)
  ntlist = sapply(ylist, nrow)
  dimdat = ncol(ylist[[1]])
  cs = c(0, cumsum(ntlist))

  ## Set up empty residual matrix (to be reused)
  cs = c(0, cumsum(ntlist))
  vars <- vector(mode = "list", numclust)
  ylong = do.call(rbind, ylist)
  ntlist = sapply(ylist, nrow)
  irows = rep(1:nrow(mn), times = ntlist)

  for(iclust in 1:numclust){
    resp.thisclust = lapply(resp, function(myresp) myresp[,iclust, drop = TRUE])
    resp.long = do.call(c, resp.thisclust)
    mnlong = mn[irows,,iclust]
    if(is.vector(mnlong)) mnlong = mnlong %>% cbind()
    ## browser()
    ## vars[[iclust]] = estepC(ylong, mnlong, sqrt(resp.long), sum(resp.long))
    ## TODO: see if this could be sped up.
    resid <- ylong - mnlong
    resid_weighted <- resp.long * resid
    sig_temp <- t(resid_weighted) %*% resid/sum(resp.long)
    vars[[iclust]] <- sig_temp
  }

  ## Make into an array
  sigma_array = array(NA, dim=c(numclust, dimdat, dimdat))
  for(iclust in 1:numclust){
    sigma_array[iclust,,] = vars[[iclust]]
  }

  ## Basic check
  stopifnot(all(dim(sigma_array) == c(numclust, dimdat, dimdat)))
  return(sigma_array)
}
