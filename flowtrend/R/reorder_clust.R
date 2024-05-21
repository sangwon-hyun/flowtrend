# Generated from _main.Rmd: do not edit by hand

#' Reorder the results of one object so that cluster 1 through
#' \code{numclust} is in a particular order. The default is decreasing order of
#' the averages (over time) of the cluster means.
#'
#' @param res Model object.
#' @param ord Defaults to NULL. Use if you have an ordering in mind.
#'
#' @return Same object, but with clusters reordered.
#'
#' @export
reorder_clust <- function(res, ord = NULL){

  ## Find an order by sums (averages)
  if(is.null(ord)) ord = res$mn[,1,] %>% colSums() %>% order(decreasing = TRUE)
  if(!is.null(ord)) all(sort(ord) == 1:res$numclust)

  ## Reorder mean
  res$mn = res$mn[,,ord, drop=FALSE]

  ## Reorder sigma
  res$sigma = res$sigma[ord,,,drop=FALSE]

  ## Reorder prob
  res$prob = res$prob[,ord, drop=FALSE]

  ## Reorder the responsibilities
  ## if('resp' %in% res){
    resp_temp = res$resp
    for(tt in 1:res$TT){
      resp_temp[[tt]] = res$resp[[tt]][,ord]
    }
  ## }
  res$resp = resp_temp
  return(res)
}
