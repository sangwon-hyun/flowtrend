# Generated from _main.Rmd: do not edit by hand

#' @param TT Number of time points.
etilde_mat <- function(TT){

  mats <- lapply(1:TT, FUN = function(t){
    e_vec <- rep(0, TT)
    e_vec[t] <- 1
    (e_vec - 1/TT) %*% t(e_vec - 1/TT)
  })
  Reduce('+', mats)
}

get_AB_mats <- function(y, resp, Sigma_inv, e_mat, Dlsqrd, N, Dlp1, Dl, rho, z, w, uz, uw){

  # A matrix
  A <- 1/N * Sigma_inv

  # B matrix
  sum_resp <- sapply(resp, sum)
  #B <- rho*(t(Dl)%*%Dl + e_mat)%*% diag(1/unlist(sum_resp))
  B <- rho*(Dlsqrd + e_mat)
  B <- B/sum_resp[col(B)]
  #B <- rho*(Dlsqrd + e_mat)  %*% diag(1/unlist(sum_resp))

  return(list(A = A, B = B))
}

get_C_mat <- function(C1, resp_sum, TT, dimdat, Sigma_inv, e_mat, N, Dlp1, Dl,
                      rho, z, w, uz, uw, l){

  C2 <- t(uz - rho*z)

  # averaging
  C2 <- C2 - rowMeans(C2)

  # third component
  C3 <- do.call(rbind, lapply(1:dimdat, FUN = function(j){
    ((uw[,j, drop=TRUE] - rho * w[,j,drop=TRUE]) %*% Dl) %>% as.numeric()
  }))

  # combining
  C <-  (-1/N * C1 + C2 + C3)
  C <- C/resp_sum[col(C)]
  return(C)
}


#' @param mat Matrix to Schur-decompose.
myschur <- function(mat){
  stopifnot(nrow(mat) == ncol(mat))
  if(is.numeric(mat) & length(mat)==1) mat = mat %>% as.matrix()
  obj = Matrix::Schur(mat)
  obj$tQ = t(obj$Q)
  obj$orig = mat
  return(obj)
}
