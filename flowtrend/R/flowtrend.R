# Generated from _main.Rmd: do not edit by hand

#' Main function. Repeats the EM algorithm (\code{flowtrend_once()}) with |nrep| restarts (5 by default).
#'
#' @param ... : arguments for \code{flowtrend_once()}
#' @param nrestart : number of random restarts
#'
#' @return
#' @export
#'
#' @examples
flowtrend <- function(..., nrestart = 5){

  args = list(...)
  if("verbose" %in% names(args)){
    if(args$verbose){
      cat("EM will restart", nrestart, "times", fill=TRUE)
    }
  }

  out_models <- lapply(1:nrestart, FUN = function(irestart){
    if("verbose" %in% names(args)){
      if(args$verbose){
        cat("EM restart:", irestart, fill=TRUE) 
      }
    }
    model_temp <- flowtrend_once(...)
    model_obj <- tail(model_temp$objectives, n = 1)
    if("verbose" %in% names(args)){
      if(args$verbose){
        cat(fill=TRUE) 
      }
    }
    return(list(model = model_temp, final_objective = model_obj))
  })
  final_objectives <- sapply(out_models, FUN = function(x) x$final_objective)
  best_model <- which.min(final_objectives)
  final_model = out_models[[best_model]][["model"]]

  ## Add the objectives
  final_model$all_objectives =
    lapply(1:nrestart, function(irestart){
        one_model = out_models[[irestart]]
        data.frame(objective=one_model$model$objectives) %>% mutate(iter=row_number(), irestart=irestart) %>% select(irestart, iter, objective)
    }) %>% bind_rows()
  return(final_model)
}
