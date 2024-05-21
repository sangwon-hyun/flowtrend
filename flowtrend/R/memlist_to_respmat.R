# Generated from _main.Rmd: do not edit by hand

#' Convert list of memberships into a list of responsibilities.
#' 
#' @param memlist List of memberships
#'
#' @return
#' @export
memlist_to_respmat <- function(memlist){
  numclust = max(sapply(memlist, max))
  respmats = lapply(memlist, function(mem){
    respmat = lapply(mem, function(onemem){
      onerow = rep(0, numclust)##c(0,0)
      onerow[onemem] = 1
      return(onerow)
    }) %>% do.call(rbind, .)
    return(respmat)
  })
  return(respmats)
}
