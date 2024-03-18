# Generated from _main.Rmd: do not edit by hand

#' A "soft" version of a rand index between two sets of responsibility
#' (membership probability) matrices.
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
    amat = resp1 %*% t(resp1)
    bmat = resp2 %*% t(resp2)
    multmat1 = amat * (1 - bmat)
    multmat2 = (1-amat) * (bmat)
    multmat1[diag(multmat1)] = 0 ## Remove diagonals
    multmat2[diag(multmat2)] = 0 ## Remove diagonals
    return(sum(multmat1) + sum(multmat2))
  }
  sum(mapply(soft_rand_onetime, resp_list1, resp_list2))
}
