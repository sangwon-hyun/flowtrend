# Generated from _main.Rmd: do not edit by hand

#' Compute KL divergence from responsibilities between two models'
#' responsibilities \code{resp_new} and \code{resp_old}.
#'
#' @param resp_new New responsibilities
#' @param resp_orig Original responsiblities.
#'
#' @return Calculate reordering \code{o} of the clusters in model represented
#'   by \code{resp_new}. To be clear, \code{o[i]} of new model is the best
#'   match with the i'th cluster of the original model.
#'
#' @export
#' @importFrom clue solve_LSAP
get_best_match_from_kl <- function(resp_new, resp_orig){

  ## Basic checks
  . = NULL ## Fixing check()
  assertthat::assert_that(all(sapply(resp_new, dim) == sapply(resp_orig , dim)))

  ## Row-bind all the responsibilities to make a long matrix
  distmat = form_symmetric_kl_distmat(resp_orig %>% do.call(rbind,.),
                                      resp_new %>% do.call(rbind,.))

  ## Use Hungarian algorithm to solve.
  fit <- clue::solve_LSAP(distmat)
  o <- as.numeric(fit)

  ## Return the ordering
  return(o)
}

##' From two probability matrices, form a (K x K) distance matrix of the
##' (n)-vectors. The distance between the vectors is the symmetric KL
##' divergence.
##'
##' @param mat1 Matrix 1 of size (n x K).
##' @param mat2 Matrix 2 of size (n x K).
##'
##' @return K x K matrix containing symmetric KL divergence of each column of
##'   \code{mat1} and \code{mat2}.
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

##' Symmetric KL divergence, of two probability vectors.
##'
##' @param vec1 First probability vector.
##' @param vec2 Second prbability vector.
##'
##' @return Symmetric KL divergence (scalar).
symmetric_kl <- function(vec1, vec2){
  stopifnot(all(vec1 <= 1) & all(vec1 >= 0))
  stopifnot(all(vec2 <= 1) & all(vec2 >= 0))
  kl <- function(vec1, vec2){
    sum(vec1 * log(vec1 / vec2))
  }
  return((kl(vec1, vec2) + kl(vec2, vec1))/2)
}
