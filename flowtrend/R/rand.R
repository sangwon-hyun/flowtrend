# Generated from _main.Rmd: do not edit by hand

#' A "soft" version of a rand index between two sets of responsibility
#' (membership probability) matrices. Measures the /disagreement/ between the two clusterings.
#' 
#' @param resp_list1 One list of responsibility matrices.
#' @param resp_list2 Another list of responsibility matrices.
#' @param times Optional; if you would like to isolate your attention to some specific times.
#'
#' @return A single soft rand index number
#' @export
rand <- function(resp_list1, resp_list2, times = NULL){
  if(!is.null(times)) resp_list1 = resp_list1[times]
  if(!is.null(times)) resp_list2 = resp_list2[times]
  
  rand_onetime <- function(resp1, resp2){
    stopifnot(nrow(resp1) == nrow(resp2))
    stopifnot(ncol(resp1) == ncol(resp2))
    nt = nrow(resp1)
    
    ## Form the score
    mat11 = resp1 %*% t(resp1)
    mat22 = resp2 %*% t(resp2)
    mat11not = (1-mat11)
    mat22not = (1-mat22)

    ## Make the diagonals not matter anywhere
    diag(mat11) = 0
    diag(mat22) = 0
    diag(mat11not) = 0
    diag(mat22not) = 0

    a = mat11 * mat22
    b = mat11not * mat22not
    c = mat11 * mat22not
    d = mat11not * mat22

    return(c(a = sum(a), b = sum(b), c = sum(c), d = sum(d)))
  }
  abcd_over_time = mapply(rand_onetime, resp_list1, resp_list2)
  abcdmat = abcd_over_time %>% t()
  abcd = abcdmat %>% colSums()
  score = (abcd["a"] + abcd["b"])/  (sum(abcd))
  return(score)
}
