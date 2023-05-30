# Generated from _main.Rmd: do not edit by hand

#' Solve a fused lasso problem for the W update. Internally, a fused lasso dynamic
#' programming solver \code{prox()} (which calls \code{prox_dp()} written in C)
#' is used.
#' 
W_update_fused <- function(lm1, TT, mu, uw, rho, lambda){

  # modified lambda for fused lasso routine
  mod_lam <- lambda/rho

  # generating pseudo response xi
  #xi <- Dlm1 %*% mu + 1/rho * uw
  if(lm1 == 0){
    xi <- mu + 1/rho * uw
  } else {
    xi <- diff(mu, differences = lm1) + 1/rho * uw
  }

  # running the fused LASSO
  ## fit <- prox(z = xi, lam = mod_lam)
  fit <- FlowTF::prox(z = xi, lam = mod_lam)

  return(fit)
}

## This function is in FlowTF now. It's the last function there!
## #' Fused LASSO for scalar inputs.
## #'
## #' @param z scalar input to be smoothed via the fused LASSO
## #' @param lam  Fused LASSO smoothing parameter
## #'
## #' @return Estimates of the fused LASSO solution
## #' @export prox
## #'
## #' @references All credit for writing this function goes to Ryan Tibshirani. See
## #'   the original code for calling this function at
## #'
## #' @useDynLib FlowTF prox_dp 
## prox <-  function(z, lam) {
##   o <- .C("prox_dp",
##           as.integer(length(z)),
##           as.double(z),
##           as.double(lam),
##           as.double(numeric(length(z))),
##           #  dup=FALSE,
##           PACKAGE="FlowTF")

##   return(o[[4]])
## }


Z_update  <- function(m, Uz, C, rho){
  mat = m + Uz/rho
  Z = projCmat(mat, C)
  return(Z)
}

projCmat <- function(mat, C){
  if(!is.null(C)){
    vlens = sqrt(rowSums(mat * mat))
    inds = which(vlens > C)
    if(length(inds) > 0){
      mat[inds,] = mat[inds,] * C / vlens[inds]
    }
  }
  return(mat)
}

U_update_Z <- function(U, rho, mu, Z){
 # return(U + rho * (scale(mu, scale = F) - Z))
  return(U + rho * (t(t(mu) - rowMeans(t(mu))) - Z))
}

U_update_W <- function(U, rho, mu, W, Dlm1, l){

  if(l==0){
    return(U +  rho * (mu - W)) ## This /seems/ like it will work.
  } else {
    return(U +  rho * ( t(diff(t(mu), differences = l)) - W))
  }
}
