# Generated from _main.Rmd: do not edit by hand

#' One cluster's admm for a fixed step size (rho).
admm_oneclust <- function(iclust = 1, niter, y,
                          Dlm1, Dl, l = NULL,
                          TT, N, dimdat, maxdev,
                          rho,
                          rhoinit = rho,
                          Xinv,
                          schurA,
                          schurB,
                          sigmainv,
                          lambda,
                          resp,
                          ylist, err_rel = 1e-3, err_abs = 0,
                          zerothresh,
                          z,
                          w,
                          uw,
                          uz,
                          warmstart = FALSE,
                          mu.warm = if(!warmstart) NULL,
                          first_iter,## Not used
                          outer_iter,
                          local_adapt,
                          sigma,
                          sigma_eig_by_clust,
                          space = 20,
                          ## diagnostics
                          norms = F, objective  = F){

  ## Initialize the variables ###
  resid_mat = matrix(NA, nrow = ceiling(niter/5), ncol = 4)
  colnames(resid_mat) = c("primresid", "primerr", "dualresid", "dualerr")

  rhofac = rho / rhoinit

  ## Main inner LA-ADMM loop
  fits = rep(NA, ceiling(niter/space))
  converge = FALSE
  start.time = Sys.time()
  Zlist = list()

  ## This doesn't change over iterations
  schurA = myschur(schurA$orig * rhofac)
  TA = schurA$T ##* rhofac
  TB = schurB$T
  UA = schurA$Q
  UB = schurB$Q
  tUA = schurA$tQ
  tUB = schurB$tQ

  ## This also doesn't change over iterations
  C1 <- do.call(cbind, lapply(1:TT, FUN = function(t){
      multmat <- apply(y[[t]], FUN = function(yy) yy*resp[[t]], MARGIN = 2)
      sigmainv %*% colSums(multmat)
    }))

  resp_sum <- lapply(resp, sum)

  for(iter in 1:niter){

    syl_C <- get_C_mat(C1 = C1, resp_sum = resp_sum, TT = TT, d = dimdat,
                       Sigma_inv = sigmainv, N = N, Dlm1 = Dlm1, rho = rho,
                       z = z, w = w, uz = uz, uw = uw)
    FF =  (-1) * tUA %*% syl_C %*% UB
    mu = UA %*% matrix_function_solve_triangular_sylvester_barebonesC2(TA, TB, FF) %*% tUB
    #mu = -1 *  matrix_function_solve_triangular_sylvester_barebones(schurA$orig, schurB$orig, syl_C)

    if(warmstart & iter == 1){
      print("warmed up mu!")
      mu = mu.warm
    }

    #z <- Z_update( scale(t(mu), scale = F), Uz = uz, C = maxdev, rho = rho)
    z <- Z_update( t(mu - rowMeans(mu)), Uz = uz, C = maxdev, rho = rho)

    wlist = lapply(1:dimdat, function(j){
      W_update_fused(lm1 = l, TT = TT, mu = mu[j,], rho = rho, lambda = lambda,
                     uw = uw[j,])})
    w <- do.call(rbind, wlist)
    uz = U_update_Z(uz, rho, t(mu), z)
    uw = U_update_W(uw, rho, mu, w, Dlm1, l = l)

    if(norms & iter %% 1 == 0){
      print(round(c("mu" = norm(mu, "F"),
                    "C" = norm(syl_C, "F"),
                    "FF" = norm(FF, "F"),
                    "z" = norm(z, "F"),
                    "w" = norm(w, "F"),
                    "uz" = norm(uz, "F"),
                    "uw" = norm(uw, "F")),3))
    }

    ## Check convergence
    if( iter > 1  & iter %% 5 == 0){## & !local_adapt){

      ## Calculate convergence criterion
      obj = converge(mu, rho,
                     w, z,
                     w_prev, z_prev,
                     uw, uz,
                     Dlm1, err_rel = err_rel, err_abs = err_abs)

      jj = (iter/ 5)
      resid_mat[jj,] = c(norm(obj$primal_resid, "F"),
                         obj$primal_err,
                         norm(obj$dual_resid,"F"),
                         obj$dual_err)

      if(is.na(obj$converge)){
        obj$converge <- FALSE
        warning("Convergence was NA")
      }
      if(obj$converge){
        converge = TRUE
        break
      }
    }

    ## ## 3. Calculate objective values for this cluster.
    w_prev = w
    z_prev = z

  }

  ## Gather results.
  mu = mu
  yhat = mu
  fit <- NULL
  fits <- NULL
  obj.value <- NULL

  if(objective){
    obj.value <- obj(y = y, mu = mu, resp = resp, Sigma_inv = sigmainv, TT = TT,
                     d = dimdat, Dl = Dl, Dlm1 = Dlm1, l = l, maxdev = maxdev,
                     lambda = lambda, rho = rho, N = N)
  }

  return(list(mu = mu,
              yhat = yhat,
              resid_mat = resid_mat,
              fits = fits,
              converge = converge,
              fit = obj.value,
              ## Other variables to return.
              Z = z,
              W = w,
              uz = uz,
              uw = uw,
              inner.iter = iter,
              objective = obj.value
  ))
}
