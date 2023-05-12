# Generated from _main.Rmd: do not edit by hand

#' Check convergence of ADMM with a fixed step size (rho).
converge <- function(mu, rho, w, z, w_prev, z_prev, uw, uz, Dlm1,
                     err_rel = 1E-4,
                     err_abs = 0
){

  prim1 = rbind(t(mu - rowMeans(mu)), Dlm1 %*% t(mu))
  prim2 = rbind(z, t(w))
  primal_resid = prim1 - prim2


  dual_resid = -rho * rbind((z-z_prev - colMeans(z-z_prev)), t(Dlm1) %*% t(w - w_prev))
  tAU = rbind(uz, -t(uw))

  ## Form primal and dual tolerances.
  primal_err = sqrt(length(primal_resid)) * err_abs +
    err_rel * max(norm(prim1, "F"), norm(prim2, "F"))
  dual_err = sqrt(length(dual_resid)) * err_abs +
    err_rel * norm(tAU, "F")

  ## Check convergence.
  primal_resid_size = norm(primal_resid, "F")
  dual_resid_size = norm(dual_resid, "F")
  primal_converge = ( primal_resid_size  <= primal_err )
  dual_converge = ( dual_resid_size <= dual_err )

  ## Some checks (trying to address problems with |converge|).
  assertthat::assert_that(is.numeric(primal_resid_size))
  assertthat::assert_that(is.numeric(primal_err))
  assertthat::assert_that(is.numeric(dual_resid_size))
  assertthat::assert_that(is.numeric(dual_err))

  ## return(primal_converge & dual_converge)
  converge = primal_converge & dual_converge

  return(list(primal_resid = primal_resid,
              primal_err = primal_err,
              dual_resid = dual_resid,
              dual_err = dual_err,
              converge = converge))
}
