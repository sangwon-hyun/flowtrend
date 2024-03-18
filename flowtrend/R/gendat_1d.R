# Generated from _main.Rmd: do not edit by hand

#' Generates some synthetic 1-dimensional data with three clusters. Returns a
#' data frame with (1) time, (2) Y (3) mu (4) cluster assignments.
#'
#' @param TT Number of time points.
#' @param nt Number of particles at each time.
#' @param offset Defaults to 0. How much to push up cluster 1.
#' @param return_model If true, return true cluster means and probabilities
#'   instead of data.
#'
#' @return long matrix with data or model.
#' @export
gendat_1d <- function(TT, ntlist, offset = 0, return_model = FALSE,
                      sd3=NULL){

  ## Basic checks
  stopifnot(length(ntlist) == TT)
  prob_link1 = rep(NA, TT)
  prob_link1[1] = 2
  for(tt in 2:floor(TT/2)){
    prob_link1[tt] = prob_link1[tt-1] + (1/TT)
  }
  for(tt in (floor(TT/2)+1):TT){
    prob_link1[tt] = prob_link1[tt-1] - .5*(1/TT)
  }
  prob_link2 = sapply(1:TT, function(tt) 3 - 0.25*(tt/TT))
  prob_link3 = sapply(1:TT, function(tt) 2.5)
  linkmat = cbind(prob_link1, prob_link2, prob_link3) ##%>% matplot()
  mysum = exp(linkmat) %>% rowSums()
  cluster_prob1 = exp(prob_link1) / mysum
  cluster_prob2 = exp(prob_link2) / mysum
  cluster_prob3 = exp(prob_link3) / mysum
  probs = cbind(cluster_prob1, cluster_prob2, cluster_prob3)
  colnames(probs) = 1:3
  sd1 = 0.4
  sd2 = 0.5
  if(is.null(sd3)) sd3 = 0.35
  ## if(!is.null(sd3)) sd3 = 1/1.5 ## The ratio of amplitude:data-standard-deviation is about
  ##             ## $1.50695$.
  probs_long = as_tibble(probs) %>%
    add_column(time = 1:TT) %>%
    pivot_longer(-"time", names_to = "cluster", values_to = "prob")
  probs_long %>% ggplot() + geom_line(aes(x=time,y=prob, group=cluster, col=cluster)) + ylim(c(0,1))

  ## Make cluster means, by time
  means <- matrix(NA, TT, 3)
  for(ii in 1:3){
    for(tt in 1:TT){
      means[tt, 1] <- offset + tt/TT - 1.5
      means[tt, 2] <- sin(seq(-1, 1, length.out = TT)[tt] * 3.1415)
      means[tt, 3] <- -3+sin(seq(-1, 1, length.out = TT)[tt] * 6.282)
    }
  }
  colnames(means) = c(1:3)
  means_long = as_tibble(means) %>%
    add_column(time = 1:TT) %>%
    pivot_longer(-"time", names_to = "cluster", values_to = "mean")
  model = full_join(means_long, probs_long, by = c("time", "cluster"))
  model = model %>% left_join(tibble(cluster = c("1", "2", "3"),
                                     sd = c(sd1, sd2, sd3)),
                              by = "cluster") ## Does this work?
  if(return_model) return(model)

  ys <- lapply(1:TT, FUN = function(tt){

    Y <- vector(mode = "list", length = 2)
    mu <- vector(mode = "list", length = 2)
    clusters_count <- rmultinom(n = 1, size = ntlist[tt], prob = probs[tt,])

    for(ii in 1:3){
      if(ii == 1){
        mn = means[tt,ii, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + rnorm(1, mean=0, sd = sd1))
        mu[[ii]] <- rep(mn, clusters_count[ii,1])
      }
      if(ii == 2){
        mn = means[tt,ii, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + rnorm(1, mean=0, sd = sd2))
        mu[[ii]] <- rep(mn, clusters_count[ii,1])
      }
      if(ii == 3){
        mn = means[tt,ii, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + rnorm(1, mean=0, sd = sd3))
        mu[[ii]] <- rep(mn, clusters_count[ii,1])
      }
    }
    Y <- unlist(Y)
    mu <- unlist(mu)
    cluster <- rep(1:3, times = clusters_count)
    one_df = tibble(time = tt, Y = Y, mu = mu, cluster = cluster)
    return(one_df)
  }) %>% bind_rows()
  return(ys)
}
