# Generated from _main.Rmd: do not edit by hand

#' Symmetric KL divergence, of two probability vectors.
#'
#' @param vec1 First probability vector.
#' @param vec2 Second prbability vector.
#'
#' @return Symmetric KL divergence (scalar).
symmetric_kl <- function(vec1, vec2){
  stopifnot(all(vec1 <= 1) & all(vec1 >= 0))
  stopifnot(all(vec2 <= 1) & all(vec2 >= 0))
  kl <- function(vec1, vec2){
    sum(vec1 * log(vec1 / vec2))
  }
  return((kl(vec1, vec2) + kl(vec2, vec1))/2)
}
