# Generated from _main.Rmd: do not edit by hand

#' Apply flowmeans sequentially and then cluster match
#' 
#' @param ylist Data.
#' @param numclust Number of clusters.
#' @return 
#' @export
overfit_flowmeans <- function(ylist, numclust, verbose = FALSE){
  
  fmns_obj <- lapply(ylist, FUN = flowmeans_each, numclust = numclust)
  hybrid_out <- match_clusters(fmns_obj)
  return(hybrid_out) 
}
