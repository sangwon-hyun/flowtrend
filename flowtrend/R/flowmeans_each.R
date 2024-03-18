# Generated from _main.Rmd: do not edit by hand

#' Apply flowmeans in each.
#' 
#' @param ylist Data.
#' @param numclust Number of clusters.
#' @return 
#' @export
flowmeans_each <- function(ylist, numclust){

  ## Get cluster labels from the peaks
  fmns_obj <- flowMeans::flowMeans(x = ylist, NumC = numclust)
  labeled_ylist <- data.frame(cluster = fmns_obj@Label, ylist)
  
  ## Calculate cluster parameters
  cluster_params <- labeled_ylist %>% group_by(cluster) %>% 
    summarise(mu = mean(Y),
              prob = n()/nrow(labeled_ylist), 
              sigma = ifelse(!is.na(var(Y)), var(Y), 1e-10))
  return(list(fmns_obj = fmns_obj, cluster_params = cluster_params))
}
