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
gendat_1d <- function(TT, ntlist, offset = 0, return_model = FALSE){

  ## Basic checks
  stopifnot(length(ntlist) == TT)

  ## Make cluster probabilities, by time
  cluster_prob1 = sapply(1:TT, function(tt) sin(tt/24 * 2 * pi)/3 + 1 + (tt/TT)*5)
  cluster_prob2 = sapply(1:TT, function(tt) cos(tt/24 * 2 * pi)/3 + 8 - (tt/TT)*5)
  cluster_prob3 = rep(3, TT)
  probs = cbind(cluster_prob1, cluster_prob2, cluster_prob3)
  probs = probs/rowSums(probs)
  colnames(probs) = 1:3
  sd1 = 0.4
  sd2 = 0.5
  sd3 = 0.35
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
