# Generated from _main.Rmd: do not edit by hand

#'  Softmax function.
#' 
#' @param prob_link alpha, which is a (T x K) matrix.
#' 
#' @return exp(alpha)/rowSum(exp(alpha)). A (T x K) matrix.
softmax <- function(prob_link){
  exp_prob_link = exp(prob_link)
  prob = exp_prob_link / rowSums(exp_prob_link)
}
