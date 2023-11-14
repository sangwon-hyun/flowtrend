# Generated from _main.Rmd: do not edit by hand

#' computes the ADMM objective for one cluster
objective_per_cluster <- function(y, TT, d, l, mu, resp, Sigma_inv, Dlp1, Dl,
                                  maxdev, lambda, rho, N){
  
  aug1 <- sum(do.call(cbind, lapply(1:TT, FUN = function(tt){
    multmat <- apply(y[[tt]], FUN = function(yy){
      t(yy - mu[tt,]) %*% Sigma_inv %*% (yy - mu[tt,])}, MARGIN = 1)
    sum(1/(2*N) * resp[[tt]] * multmat)
  })))

  aug2 <- lambda*sum(do.call(rbind, lapply(1:d, FUN = function(j)
    sum(abs(diff(mu[,j], differences = l))))))

  total <- aug1 + aug2
  return(total)
}
