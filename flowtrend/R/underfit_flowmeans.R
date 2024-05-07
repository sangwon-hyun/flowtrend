# Generated from _main.Rmd: do not edit by hand

#' Pool the entire series of cytograms, fit a flowMeans model, and re-aggregate parameters
#' 
#' @param ylist Data.
#' @param numclust Number of clusters.
#' @return List object with flowtrend model estimates using l=lprob=0 and
#'   extremely large regularization parameters so that all cytograms are the
#'   same across time.
#' @export
underfit_flowmeans <- function(ylist, numclust){

  ## Pool all data
  TT <- length(ylist)
  nt <- sapply(ylist, nrow)
  ylist_pooled <- do.call(rbind, ylist) %>% as_tibble()
  colnames(ylist_pooled) = "Y"

  ## Fit the model
  fmns_pooled <- flowMeans::flowMeans(x = ylist_pooled, NumC = numclust)
  labeled_ylist <- data.frame(Y = ylist_pooled$Y,
                              Cluster = fmns_pooled@Label,
                              Time = rep(1:TT, times = nt))

  ## Separate out list of memberships
  memlist = labeled_ylist %>% group_by(Time) %>% group_split() %>% lapply(function(a)pull(a, Cluster))
  
  ## Format nicer output for objective calculations
  params <- labeled_ylist %>% group_by(Cluster, Time) %>% 
    summarise(mu = mean(Y), prob = n(), sigma = var(Y)) %>% 
    group_by(Time) %>% mutate(prob = prob/sum(prob))
  mu <- matrix(nrow = TT, ncol = numclust)
  sigma <- matrix(nrow = TT, ncol = numclust)
  prob <- matrix(nrow = TT, ncol = numclust)
  
  ## Fill in missing times for some clusters
  for(tt in 1:TT){
    
    params.tt <- params %>% filter(Time == tt)
    for(iclust in 1:numclust){
      
      mu[tt,iclust] <- params.tt$mu[iclust]
      sigma[tt,iclust] <- params.tt$sigma[iclust]
      prob[tt,iclust] <- params.tt$prob[iclust]
      
      if(is.na(mu[tt,iclust])){
        mu[tt,iclust] <- mu[tt-1,iclust]
      }
      if(is.na(sigma[tt,iclust])){
        sigma[tt,iclust] <- sigma[tt-1,iclust]
      }
      if(is.na(prob[tt,iclust])){
        prob[tt,iclust] <- 0
      }
    }
  }
  return(list(labeled_ylist = labeled_ylist,
              memlist = memlist,
              params = params,
              mu = mu,
              prob = prob,
              sigma = sigma,
              numclust = numclust))
}
