# Generated from _main.Rmd: do not edit by hand

#' From two probability matrices, form a (K x K) distance matrix of the
#' (n)-vectors. The distance between the vectors is the symmetric KL
#' divergence.
#'
#' @param mat1 Matrix 1 of size (n x K).
#' @param mat2 Matrix 2 of size (n x K).
#'
#' @return K x K matrix containing symmetric KL divergence of each column of
#'   \code{mat1} and \code{mat2}.
form_symmetric_kl_distmat <- function(mat1, mat2){

  ## Manually add some small, in case some columns are all zero
  mat1 = (mat1 + 1E-10) %>% pmin(1)
  mat2 = (mat2 + 1E-10) %>% pmin(1)

  ## Calculate and return distance matrix.
  KK1 = ncol(mat1)
  KK2 = ncol(mat2)
  distmat = matrix(NA, ncol=KK2, nrow=KK1)
  for(kk1 in 1:KK1){
    for(kk2 in 1:KK2){
      mydist = symmetric_kl(mat1[,kk1, drop=TRUE], mat2[,kk2, drop=TRUE])
      distmat[kk1, kk2] = mydist
    }
  }
  stopifnot(all(!is.na(distmat)))
  return(distmat)
}
