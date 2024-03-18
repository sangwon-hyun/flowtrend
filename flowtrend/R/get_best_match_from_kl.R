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
