# Generated from _main.Rmd: do not edit by hand

#' Define the time folds cross-validation.
#'
#' @param nfold Number of folds.
#' @return List of fold indices.
#' @export
#'
make_cv_folds <- function(ylist=NULL, nfold, TT=NULL){

  ## Make hour-long index list
  if(is.null(TT)) TT = length(ylist)
  folds <- rep(1:nfold, ceiling( (TT-2)/nfold))[1:(TT-2)]
  inds <- lapply(1:nfold, FUN = function(k) (2:(TT-1))[folds == k])
  names(inds) = paste0("Fold", 1:nfold)

  return(inds)
} 
