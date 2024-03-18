# Generated from _main.Rmd: do not edit by hand

#' Generates some synthetic 3-dimensional data with three clusters. 
#'
#' @param TT Number of time points.
#' @param nt Number of particles at each time.
#'
#' @return List containing (1) ylist, (2) mnlist, (3) clusterlist.
#' @export
gendat_3d <- function(TT, ntlist){

  ## Basic checks
  stopifnot(length(ntlist) == TT)

  ## Make cluster probabilities, by time
  cluster_prob1 = sapply(1:TT, function(tt) sin(tt/24 * 2 * pi)/3 + 1 + (tt/TT)*5)
  cluster_prob2 = sapply(1:TT, function(tt) cos(tt/24 * 2 * pi)/3 + 8 - (tt/TT)*5)
  cluster_prob3 = rep(3, TT)
  probs = cbind(cluster_prob1, cluster_prob2, cluster_prob3)
  probs = probs/rowSums(probs)
  colnames(probs) = 1:3
  probs_long = as_tibble(probs) %>%
    add_column(time = 1:TT) %>%
    pivot_longer(-"time", names_to = "cluster", values_to = "prob")
  probs_long %>% ggplot() + geom_line(aes(x=time,y=prob, group=cluster, col=cluster)) + ylim(c(0,1))

  ## Make cluster means, by time
  numclust = 3
  dimdat = 3
  ## means <- array(NA, dim = c(TT, 3, 2))
  means <- array(NA, dim = c(TT, numclust, dimdat))
  for(ii in 1:3){
    for(tt in 1:TT){

      ## First cluster 
      means[tt, 1, 1] = means[tt, 1, 2] = means[tt, 1, 3] = tt/TT + 0.5

      ## Second cluster 
      means[tt, 2, 1]  = sin(tt/24 * 2 * pi)##seq(-1, 1, length.out = TT)[tt]*3.1415)
      means[tt, 2, 2] = 0
      means[tt, 2, 3] = 0

      ## Third cluster
      means[tt, 3, 1] = means[tt, 3, 2] = means[tt, 3, 3] =
        -3+cos(tt/24 * 2 * pi)##seq(-1, 1, length.out = TT)[tt]*6.282)
    }
  }
  dimnames(means)[[2]]  = c(1:3)
  means_long = as_tibble(means) %>%
    add_column(time = 1:TT) %>%
    pivot_longer(-"time", names_to = "cluster", values_to = "mean")
  model = full_join(means_long, probs_long, by = c("time", "cluster"))

  ylist = list()
  mulist = list()
  clusterlist = list()
  for(tt in 1:TT){
    Y <- vector(mode = "list", length = numclust)
    mu <- vector(mode = "list", length = numclust)
    clusters_count <- rmultinom(n = 1, size = ntlist[tt], prob = probs[tt,])

    for(ii in 1:3){
      if(ii == 1){
        mn = means[tt,ii,,drop=TRUE]
        Sigma1 = matrix(rep(0.3, 9), ncol = 3)
        diag(Sigma1) = 0.4
        ## Sigma1 = matrix(c(0.4, 0.3, 0.3, 0.4), ncol = 2)
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + MASS::mvrnorm(1, mu=c(0,0,0), Sigma= Sigma1)) %>% t()
        mu[[ii]] <- replicate(clusters_count[ii,1], mn) %>% t()
      }
      if(ii == 2){
        mn = means[tt,ii,, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + MASS::mvrnorm(1, mu=c(0,0,0), Sigma = diag(c(0.5, 0.1, 0.2)))) %>% t()
        mu[[ii]] <- replicate(clusters_count[ii,1], mn) %>% t()
      }
      if(ii == 3){
        mn = means[tt,ii,, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + MASS::mvrnorm(1, mu=c(0,0,0), Sigma = diag(c(0.35, 0.35, 0.35)))) %>% t()
        mu[[ii]] <- replicate(clusters_count[ii,1], mn) %>% t()
      }
    }

    Y <- Y %>% purrr::compact() %>% do.call(rbind, .)
    mu <- mu %>% purrr::compact() %>% do.call(rbind, .)
    cluster <- rep(1:3, times = clusters_count)
    colnames(Y) = paste0("dim", 1:3)
    ylist[[tt]] = Y
    mulist[[tt]] = mu
    clusterlist[[tt]] = cluster
  }
  return(list(ylist = ylist, mulist = mulist,
              clusterlist = clusterlist,
              probs = probs, means = means))
}
