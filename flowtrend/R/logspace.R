# Generated from _main.Rmd: do not edit by hand

#' Helper function to logarithmically space out R.  \code{length} values linear
#' on the log scale from \code{max} down to \code{min}.
#'
#' @param max Maximum value.
#' @param min Minimum value.
#' @param length Length of the output string.
#' @param min.ratio Factor to multiply to \code{max}.
#'
#' @return Log spaced
#'
#' @export
logspace <- function(max, min=NULL, length, min.ratio = 1E-4){
  if(is.null(min)) min = max * min.ratio
  vec = 10^seq(log10(min), log10(max), length = length)
  stopifnot(abs(vec[length(vec)] - max) < 1E10)
  return(vec)
}
