# Generated from _main.Rmd: do not edit by hand

#' Generating Difference Matrix of Specified Order
#'
#' @param n length of vector to be differenced
#' @param l order of differencing
#' @param x optional. Spacing of input points.
#'
#' @return A n by n-l-1 matrix
#' @export
#'
#' @examples
gen_diff_mat <- function(n, l, x = NULL){

  ## Basic check
  if(!is.null(x))  stopifnot(length(x) == n) 
  if(is.unsorted(x))  stop("x must be in increasing order!") 

  get_D1 <- function(t) {do.call(rbind, lapply(1:(t-1), FUN = function(x){
    v <- rep(0, t)
    v[x] <- -1
    v[x+1] <- 1
    v
  }))}

  if(is.null(x)){
    if(l == 0){
      return(diag(rep(1,n)))
    }
    if(l == 1){
      return(get_D1(n))
    }
    if(l > 1){
      D <- get_D1(n)
      for(k in 1:(l-1)){
        D <- get_D1(n-k) %*% D
      }
      return(D)
    }
  }
  else{
    if(l == 0){
      return(diag(rep(1,n)))
    }
    if(l == 1){
      return(get_D1(n))
    }
    if(l > 1){
      D <- get_D1(n)
      for(k in 1:(l-1)){
        D <- get_D1(n-k) %*% diag(k/diff(x, lag = k)) %*% D
      }
      return(D)
    }
  }
}
