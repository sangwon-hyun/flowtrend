# Generated from _main.Rmd: do not edit by hand

#' Main function for summarizing the cross-validation results. 
#'
#' @inheritParams cv_flowtrend
#' @param save If TRUE, save to \code{file.path(destin, filename)}.
#' @param filename File name to save to.
#'
#' @return List containing summarized results from cross-validation. Here are
#'   some objects in this list: \code{bestres} is the the overall best model
#'   chosen from the cross-validation; \code{cvscoremat} is a 2d matrix of CV
#'   scores from all pairs of regularization parameters; \code{bestreslist} is a
#'   list of all the best models (out of \code{nrestart} EM replications) from the
#'   each pair of lambda values. If \code{isTRUE(save)}, nothing is returned.
#'
#' @export
cv_summary <- function(destin = ".",
                       save = FALSE,
                       filename = "summary.RDS"){
  
  load(file.path(destin,'meta.Rdata'))

  ## This loads all the necessary things: nrestart, nfold, cv_gridsize
  stopifnot(exists("nrestart"))
  stopifnot(exists("nfold"))
  stopifnot(exists("cv_gridsize"))

  ## Get the results of the cross-validation.
  a = cv_aggregate(destin)
  cvscore.mat = a$cvscore.mat
  min.inds = a$min.inds

  ## Get results from refitting
  bestreslist = cv_aggregate_res(destin = destin)
  bestres = bestreslist[[paste0(min.inds[1] , "-", min.inds[2])]]
  if(is.null(bestres)){
    if(min.inds[1]==2 & min.inds[2]==3) browser()
    stop(paste0("The model with lambda indices (",
                min.inds[1], ",", min.inds[2], ") is not available."))
  }
  out = list(bestres = bestres,
             cvscore.mat = cvscore.mat,
             min.inds = min.inds,
             lambda_means = lambda_means,
             lambda_probs = lambda_probs,
             ## List of all best models for all lambda pairs.
             bestreslist = bestreslist,
             destin = destin)

  if(save){
    saveRDS(out, file=file.path(destin, filename))
  }
  return(out)
}
