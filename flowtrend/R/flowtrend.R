# Generated from _main.Rmd: do not edit by hand

#' Main function. Repeats the EM algorithm (\code{flowtrend_once()}) with |nrep| restarts (5 by default).
#'
#' @param nrestart : number of random restarts
#' @param ... : arguments for \code{flowtrend_once()}
#'
#' @return
#' @export
#'
#' @examples
flowtrend <- function(nrestart = 5, ...){
  out_models <- lapply(1:nrestart, FUN = function(x){
    model_temp <- flowtrend_once(...)
    model_obj <- tail(model_temp$objectives, n = 1)
    return(list(model = model_temp, final_objective = model_obj))
  })
  final_objectives <- sapply(out_models, FUN = function(x) x$final_objective)
  best_model <- which.min(final_objectives)
  final_model = out_models[[best_model]][["model"]]
  return(final_model)
}
