# Generated from _main.Rmd: do not edit by hand

#' A "soft" version of a rand index between two sets of responsibility
#' (membership probability) matrices. Measures the /disagreement/ between the two clusterings.
#' 
#' @param resp_list1 One list of responsibility matrices.
#' @param resp_list2 Another list of responsibility matrices.
#'
#' @return A single soft rand index number
#' @export
soft_rand <- function(resp_list1, resp_list2){
  
  soft_rand_onetime <- function(resp1, resp2){
    stopifnot(nrow(resp1) == nrow(resp2))
    stopifnot(ncol(resp1) == ncol(resp2))
    nt = nrow(resp1)
    
    ## Form the score
    mat11 = resp1 %*% t(resp1)
    mat22 = resp2 %*% t(resp2)

    a = mat11 * mat22
    b = (1-mat11) * (1-mat22)
    c = mat11 * (1-mat22)
    d = (1-mat11) * mat22
    return(c(a = sum(a), b = sum(b), c = sum(c), d = sum(d)))
  }
  abcd_over_time = mapply(soft_rand_onetime, resp_list1, resp_list2)
  abcdmat = abcd_over_time %>% t()
  abcd = abcdmat %>% colSums()
  score = (abcd["a"] + abcd["b"])/  (sum(abcd))
  return(score)
}
