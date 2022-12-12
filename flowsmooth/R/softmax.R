#' Converts the Xbeta to softmax(Xbeta), so to speak. Xbeta is the linear functional of X from a multinomial regression; in our notation, it's alpha.
#' 
#' @param prob_link alpha, which is a (T x K) matrix.
#' 
#' @return exp(alpha)/rowSum(exp(alpha)). A (T x K) matrix.
softmax <- function(prob_link){
  exp_prob_link = exp(prob_link)
  prob = exp_prob_link / rowSums(exp_prob_link)
}
