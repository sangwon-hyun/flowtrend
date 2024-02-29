# Generated from _main.Rmd: do not edit by hand

#' Define the "blocked" time folds for cross-validation.
#' This means that contiguous of times will be used to define CV folds.
#'
#' The first fold will be ( 1 2 3 16 17 18 31 32 33 46 47 48 61 62 63 76 77 78
#' 91 92 93), the second fold will be (4 5 6 19 20 21 34 35 36 49 50 51 64 65 66
#' 79 80 81 94 95 96), and so forth.
#'
#' @param nfold Number of folds.
#' @param blocksize Size of block (e.g. 3 will produce the example above).
#' @return List of fold indices.
#' @export
#'
make_cv_folds_in_blocks <- function(ylist=NULL, nfold, TT=NULL, blocksize){

  ## Make hour-long index list
  if(is.null(TT)) TT = length(ylist)
  endpoints = round(seq(from = 1, to = TT + blocksize,
                        by = blocksize))
  inds = Map(function(begin, end){
    if(begin >= TT-1) return(NULL)
    return(seq(begin+1, pmin(end,TT-1)))
  }, endpoints[-length(endpoints)],  endpoints[-1])
  null.elt = sapply(inds, is.null)
  if(any(null.elt)){
    inds = inds[-which(null.elt)]
  }

  ## Further make these into (e.g. 5) blocks of test indices.
  test.ii.list = lapply(1:nfold, function(ifold){
    which.test.inds = seq(from = ifold, to = length(inds), by = nfold)
    test.ii = unlist(inds[which.test.inds])
    return(test.ii)
  })
  names(test.ii.list) = paste0("Fold", 1:nfold)

  ## Useful plotting code showing the plots.
  if(FALSE){
    plot(NA, xlim = c(0,TT), ylim=1:2)
    lapply(1:nfold, function(ifold){
      a = test.ii.list[[ifold]]; abline(v=a, col=ifold)
    })
  }
  return(test.ii.list)
} 
