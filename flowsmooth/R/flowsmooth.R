# Generated from _main.Rmd: do not edit by hand

#' Main function. Repeats the EM algorithm (\code{flowsmooth_once()}) with |nrep| restarts (5 by default).
#'
#' @param nrestart : number of random restarts
#' @param ... : arguments for \code{flowsmooth_once()}
#'
#' @return
#' @export
#'
#' @examples
flowsmooth <- function(nrestart = 10, ...){
  out.models <- lapply(1:nrestart, FUN = function(x){
    model.temp <- flowsmooth_once(...)
    model.obj <- tail(model.temp$objectives, n = 1)
    return(list(model = model.temp, objective = model.obj))
  })
  objectives <- sapply(out.models, FUN = function(x) x$objective)
  best.model <- which.min(objectives)
  return(out.models[[best.model]][["model"]])
}
