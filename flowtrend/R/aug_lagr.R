# Generated from _main.Rmd: do not edit by hand

#' computes the Augmented lagrangian.
aug_lagr <- function(y, TT, d, z, w, l, uz, uw, mu, resp, Sigma_inv, Dl, Dlm1, maxdev, lambda, rho, N){
  mu_dd <- rowMeans(mu)

  # Check the Z's for ball constraint, up to a tolerance of 1e-4
  znorms <- apply(z, FUN = function(zz) sqrt(sum(zz^2)), MARGIN = 1)
  if(any(is.na(znorms))) browser()
  if(any(znorms > (maxdev + 1e-4))){
    warning("||z||_2 > maxdev, current iterate not feasible.")
    return(Inf)
  }

  aug1 <- sum(do.call(cbind, lapply(1:TT, FUN = function(t){
    multmat <- apply(y[[t]], FUN = function(yy){
      t(yy - mu[,t]) %*% Sigma_inv %*% (yy - mu[,t])}, MARGIN = 1)
    sum(1/(2*N) * resp[[t]]*multmat)
  })))

  aug2 <- lambda*sum(do.call(rbind, lapply(1:d, FUN = function(j)
    sum(abs(diff(w[j,], differences = 1))))))

  aug3 <- sum(do.call(cbind, lapply(1:TT, FUN = function(t){
    uz[t,]%*%(mu[,t] - mu_dd - z[t,]) + rho/2 * sum((mu[,t] - mu_dd - z[t,])^2)
  })))


  aug4 <- sum(do.call(rbind, lapply(1:d, FUN = function(j){
    uw[j,] %*% (Dlm1 %*% mu[j,]  - w[j,]) + rho/2 * sum((Dlm1 %*% mu[j,] - w[j,])^2)
  })))

  total <- aug1 + aug2 + aug3 + aug4

  return(total)
}
