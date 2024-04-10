# Generated from _main.Rmd: do not edit by hand

#' Reordering function
reorder_flowmeans <- function(obj, new_order){
  numclust = max(new_order)
  obj$prob = obj$prob[,new_order]
  obj$mu = obj$mu[,new_order]
  obj$sigma = obj$sigma[,new_order]
  obj$memlist =  lapply(obj$memlist, function(a){
    a_copy = a
    for(iclust in 1:numclust){
      a_copy[which(a==iclust)] = new_order[iclust]
    }
      ## a[a==1] = new_order[1]
      ## a[a==2] = new_order[2] ## 3
      ## a[a==3] = new_order[3] ## 2
    return(a_copy)
  })
  return(obj)
}
