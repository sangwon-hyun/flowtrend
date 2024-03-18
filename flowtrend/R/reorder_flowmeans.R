# Generated from _main.Rmd: do not edit by hand

#' Reordering function
reorder_flowmeans <- function(obj, new_order){
  obj$prob = obj$prob[,new_order]
  obj$mu = obj$mu[,new_order]
  obj$sigma = obj$sigma[,new_order]
  obj$relabels =  lapply(obj$relabels, function(a){
    a[a==1] = new_order[1]
    a[a==2] = new_order[2] ## 3
    a[a==3] = new_order[3] ## 2
    return(a)
  })
  return(obj)
}
