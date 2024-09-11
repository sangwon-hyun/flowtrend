# Generated from _main.Rmd: do not edit by hand

#' A "soft" *and adjusted* version of a rand index between two lists of responsibility
#' (membership probability) matrices. Measures the /disagreement/ between the two clusterings.
#' 
#' @param resp_list1 One list of responsibility matrices.
#' @param resp_list2 Another list of responsibility matrices.
#' @param times Optional; if you would like to isolate your attention to some
#'   specific times.
#'
#' @return A single soft rand index for all particles across all times.
#' 
#' @export
adjusted_rand <- function(resp_list1, resp_list2, times = NULL){ 

  ## Basic checks
  if(!is.null(times)) resp_list1 = resp_list1[times]
  if(!is.null(times)) resp_list2 = resp_list2[times]
  
  all_mat_over_time = mapply(adjusted_rand_onetime, resp_list1, resp_list2,
                             SIMPLIFY = FALSE)
  mat = Reduce('+', all_mat_over_time)

  n = sum(mat)
  sum_a = sum(sapply(rowSums(mat), choose, 2))
  sum_b = sum(sapply(colSums(mat), choose, 2))
  numer = sum(sapply(mat, function(a) choose(a,2))) -
    sum_a * sum_b / choose(n, 2)
  denom = (sum_a + sum_b)/2 - (sum_a * sum_b)/choose(n,2) 
  ari = numer/denom
  return(ari)
}


#' A "soft" *and adjusted* version of a rand index between two responsibility
#' (membership probability) matrices. Measures the /disagreement/ between the
#' two clusterings.
#' 
#' @param resp1 One responsibility matrices.
#' @param resp2 Another responsibility matrices.
#'
#' @return A single soft rand index for all particles.
#' @export
adjusted_rand_onetime <- function(resp1, resp2){

  ## Basic checks
  stopifnot(nrow(resp1) == nrow(resp2))
  stopifnot(ncol(resp1) == ncol(resp2))
  nt = nrow(resp1)
  
  ## Next, build contingency table
  mem1 = resp1 %>% apply(1, function(a){(1:2)[which(a==0)]}) ## why is this a==0?? TODO address this.
  mem2 = resp2 %>% apply(1, function(a){(1:2)[which(a==0)]})
  mat = matrix(NA, nrow=2, ncol=2)
  mat[1,1] = sum(mem1==1 & mem2 == 1) 
  mat[2,2] = sum(mem1==2 & mem2 == 2) 
  mat[1,2] = sum(mem1==1 & mem2 == 2) 
  mat[2,1] = sum(mem1==2 & mem2 == 1) 
  return(mat)
}
