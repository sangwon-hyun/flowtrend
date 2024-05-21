# Generated from _main.Rmd: do not edit by hand

#' @param U (T x dimdat) matrix.
U_update_Z <- function(U, rho, mu, Z, TT){
 # return(U + rho * (scale(mu, scale = F) - Z))
  stopifnot(nrow(U) == TT)

  centered_mu = sweep(mu, 2, colMeans(mu)) 
  ## stopifnot(all(abs(colMeans(centered_mu))<1E-8))
  Unew = U + rho * (centered_mu - Z)

  ## Expect a (T-l) x dimdat matrix.
  stopifnot(all(dim(U) == dim(Unew))) 
  stopifnot(nrow(U) == TT)
  return(Unew)
}

#' @param U ((T-l) x dimdat) matrix.
U_update_W <- function(U, rho, mu, W, l, Dl, TT){

  # l = 2 is quadratic trend filtering 
  # l = 1 is linear trend filtering
  # l = 0 is fused lasso
  # D^{(1)} is first differences, so it correponds to l=0
  # D^{(l+1)} is used for order-l trend filtering.
  stopifnot(nrow(W) == TT - l)
  ## if(l == 0){
  ##   Unew = U +  rho * (mu - W)
  ## } else {
  ##   Unew = U + rho * ( diff(mu, differences = l) - W)
  ## }
  Unew <- U + rho * (Dl %*% mu - W)

  ## Expect a (T-l) x dimdat matrix.
  stopifnot(all(dim(U) == dim(Unew))) 
  stopifnot(nrow(U) == TT-l)
  return(Unew)
}
