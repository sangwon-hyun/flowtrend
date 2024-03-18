# Generated from _main.Rmd: do not edit by hand

#' Apply the hungarian matching algorithm to each time point.
#'
#' @param flowmeans_obj
#' 
#' @return
match_clusters <- function(fmns_obj){
  
  ## Initialize some objects
  TT <- length(fmns_obj)
  numc <- nrow(fmns_obj[[1]]$cluster_params)
  mu <- matrix(nrow = TT, ncol = numc)
  sigma <- matrix(nrow = TT, ncol = numc)
  prob <- matrix(nrow = TT, ncol = numc)
  costs <- rep(NA, TT)
  relabels <- list(fmns_obj[[1]]$fmns_obj@Label)
  
  ## First row is spoken for
  mu[1,] <- fmns_obj[[1]]$cluster_params$mu
  sigma[1,] <- fmns_obj[[1]]$cluster_params$sigma
  prob[1,] <- fmns_obj[[1]]$cluster_params$prob
  
  ## Fill in the rest of the rows by sequentially matching clusters from t-1 to
  ## t, using the Hungarian Algorithm
  for(tt in 2:TT){
    dt_to_tp1 <- my_symmetric_kl(c1 = fmns_obj[[tt-1]]$cluster_params,
                              c2 = fmns_obj[[tt]]$cluster_params)
    hng_tt <- RcppHungarian::HungarianSolver(dt_to_tp1)
    hng_order <- hng_tt$pairs %>% data.frame() %>% arrange(.[,2]) %>% .[,1]

    fmns_obj[[tt]]$cluster_params <- fmns_obj[[tt]]$cluster_params[hng_order,]
    mu[tt,] <- fmns_obj[[tt]]$cluster_params$mu
    prob[tt,] <- fmns_obj[[tt]]$cluster_params$prob
    sigma[tt,] <- fmns_obj[[tt]]$cluster_params$sigma[hng_order]
    costs[tt] <- hng_tt$cost
    if(mean(abs(mu[tt,] - mu[tt-1,]))/max(abs(mu[tt,])) > 1){
      cat(paste("Bad Match at time", tt, "\n"))
    }

    ## Convert labels
    label.convert <- function(c1){hng_tt$pairs[c1,2]}
    relabels[[tt]] <- sapply(fmns_obj[[tt]]$fmns_obj@Label, label.convert)
  }
  
  return(list(mu = mu, prob = prob, sigma = sigma, costs = costs, relabels = relabels))
}

#' Given two K x 2 columns with parameters, make K x K distance matrix.
#' 
#' @param c1 Data frame with K rows and 2 columns (mu and sigma)
#' @param c2 Another data frame of the same size as \code{c1}.
#'
#' @return K x K matrix.
my_symmetric_kl <- function(c1, c2){
  
  ## Make sure were using the same number of clusters
  assertthat::assert_that(nrow(c1) == nrow(c2))
  numclust <- nrow(c1)

  ## Fill out distance matrix
  dist_cs <- matrix(0, ncol = numclust, nrow = numclust)
  for(iclust_1 in 1:numclust){
    for(iclust_2 in 1:numclust){
      dist_cs[iclust_2, iclust_1] <-
        one_symmetric_kl(mu1 = c1$mu[iclust_1],
                         mu2 = c2$mu[iclust_2], 
                         sigma1 = sqrt(c1$sigma[iclust_1]),
                         sigma2 = sqrt(c2$sigma[iclust_2]))
    }
  }
  rownames(dist_cs) <- rep("C1", numclust)
  colnames(dist_cs) <- rep("C2", numclust)
  return(dist_cs)
}

#' Symmetric KL divergence between two Gaussian distributions.
#' @param mu1 Mean for distribution 1.
#' @param mu2 Mean for distribution 2.
#' @param sigma1 Standard deviation for distribution 1.
#' @param sigma2 Standard deviation for distribution 2.
one_symmetric_kl <- function(mu1, mu2, sigma1, sigma2){
  stopifnot(length(mu1)==1 & length(mu2) == 1 &
            length(sigma1) == 1 & length(sigma2) == 1)
  kl12 <- log(sigma2/sigma1) + (sigma1^2 + (mu1 - mu2)^2)/(2*sigma2^2) - 1/2
  kl21 <- log(sigma1/sigma2) + (sigma2^2 + (mu2 - mu1)^2)/(2*sigma1^2) - 1/2
  kl_sym <- 0.5*(kl12 + kl21)
  return(kl_sym)
}
