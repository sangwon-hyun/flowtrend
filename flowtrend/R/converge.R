# Generated from _main.Rmd: do not edit by hand

#' Check convergence of ADMM with a fixed step size (rho).
converge <- function(mu, rho, w, z, w_prev, z_prev, uw, uz, Dl, tDl,
                     err_rel = 1E-4,
                     err_abs = 0
){

  ## Constraints are: Ax + Bz =c, where x = mu, z =(w, z)
  prim1 = rbind(Dl %*% mu, mu - colMeans(mu))
  prim2 = rbind(-w, -z)
  primal_resid = prim1 + prim2   ## Ax + Bz - c

  change_z = z - z_prev
  change_w = w - w_prev
  dual_resid = rho * (-(change_z - colMeans(change_z)) - tDl %*% change_w)
  tAU = (uz - colMeans(uz)) + tDl %*% uw
         
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

  return(list(
      primal_resid1 = prim1,
      primal_resid2 = prim2,
      primal_resid = primal_resid,
      primal_err = primal_err,
      dual_resid = dual_resid,
      dual_err = dual_err,
      converge = converge))
}
