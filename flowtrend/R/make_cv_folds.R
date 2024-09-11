# Generated from _main.Rmd: do not edit by hand

#' Define the time folds cross-validation.
#'
#' @param ylist Data.
#' @param TT Length of data; if provided, ylist is ignored.
#' @param nfold Number of folds.
#' @param blocksize Defaults to 1. If larger than 1, creates a set of time folds
#'   that use contiguous time blocks (by calling
#'   \code{make_cv_folds_in_blocks()}).
#' @return List of fold indices.
#' @export
#'
make_cv_folds <- function(ylist = NULL, nfold, TT = NULL, blocksize = 1){

  if(blocksize > 1){
    return(make_cv_folds_in_blocks(ylist = ylist,
                                   nfold,
                                   TT = TT,
                                   blocksize = blocksize))
  }

  ## Make hour-long index list
  if(is.null(TT)) TT = length(ylist)
  folds <- rep(1:nfold, ceiling( (TT-2)/nfold))[1:(TT-2)]
  inds <- lapply(1:nfold, FUN = function(k) (2:(TT-1))[folds == k])
  names(inds) = paste0("Fold", 1:nfold)

  return(inds)
} 
