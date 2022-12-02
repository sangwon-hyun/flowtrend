# Generated from _main.Rmd: do not edit by hand

#' Generates some synthetic 1-dimensional data with three clusters. Returns a
#' data frame with (1) time, (2) Y (3) mu (4) cluster assignments.
#'
#' @param TT Number of time points.
#' @param nt Number of particles at each time.
#' @param die_off_time When the cluster probability dies off, in the
#'   middle. For instance, 0.45 means it dies off at time \code{.45*TT}, then
#'   lives again at \code{(1 - .45)*TT}.
#' @param return_model If true, return true cluster means and probabilities
#'   instead of data.
#'
#' @return long matrix with data or model.
#' @export
gendat_1d <- function(TT, ntlist, die_off_time = 0.45,
                      return_model = FALSE){

  ## Make cluster probabilities, by time
  probs <- sapply(1:TT, FUN = function(tt){
    if(TT * die_off_time < tt & tt < (1-die_off_time) * TT){
      cluster_prob <- c(0.05, 0.95*rep(1/(3-1), 3-1))
    } else {
      cluster_prob <- rep(1/3, 3)
    }
    return(cluster_prob)
  }) %>% t()
  colnames(probs) = 1:3
  probs_long = as_tibble(probs) %>%
    add_column(time = 1:TT) %>%
    pivot_longer(-"time", names_to = "cluster", values_to = "prob")

  ## Make cluster means, by time
  means <- matrix(NA, TT, 3)
  for(ii in 1:3){
    for(tt in 1:TT){
      means[tt, 1] <- tt/TT + 0.5
      means[tt, 2] <- sin(seq(-1, 1, length.out = TT)[tt]*3.1415)
      means[tt, 3] <- -3+sin(seq(-1, 1, length.out = TT)[tt]*6.282)
    }
  }
  colnames(means) = c(1:3)
  means_long = as_tibble(means) %>%
    add_column(time = 1:TT) %>%
    pivot_longer(-"time", names_to = "cluster", values_to = "mean")
  model = full_join(means_long, probs_long, by = c("time", "cluster"))
    ## mutate(cluster = as.numeric(cluster))
  if(return_model) return(model)

  ys <- lapply(1:TT, FUN = function(tt){

    Y <- vector(mode = "list", length = 2)
    mu <- vector(mode = "list", length = 2)
    clusters_count <- rmultinom(n = 1, size = ntlist[tt], prob = probs[tt,])

    for(ii in 1:3){
      if(ii == 1){
        mn = means[tt,ii, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + rnorm(1, sd = 0.4))
        mu[[ii]] <- rep(mn, clusters_count[ii,1])
      }
      if(ii == 2){
        mn = means[tt,ii, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + rnorm(1, sd = .5))
        mu[[ii]] <- rep(mn, clusters_count[ii,1])
      }
      if(ii == 3){
        mn = means[tt,ii, drop=TRUE]
        Y[[ii]] <- replicate(clusters_count[ii,1], mn + rnorm(1, sd = .35))
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
