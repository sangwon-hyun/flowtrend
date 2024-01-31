# Generated from _main.Rmd: do not edit by hand

#' Estimate flowtrend model once.
#'
#' @param ylist Data.
#' @param countslist Counts corresponding to multiplicities.
#' @param x Times, if points are not evenly spaced. Defaults to NULL, in which
#'   case the value becomes \code{1:T}, for the $T==length(ylist)$.
#' @param numclust Number of clusters.
#' @param niter Maximum number of EM iterations.
#' @param l Degree of differencing for the mean trend filtering. l=0 will give
#'   you piecewise constant means; l=1 is piecewise linear, and so forth. 
#' @param l_prob Degree of differencing for the probability trend filtering. 
#' @param mn Initial value for cluster means. Defaults to NULL, in which case
#'   initial values are randomly chosen from the data.
#' @param lambda Smoothing parameter for means
#' @param lambda_prob Smoothing parameter for probabilities
#' @param verbose Loud or not? EM iteration progress is printed.
#' @param tol_em Relative numerical improvement of the objective value at which
#'   to stop the EM algorithm
#' @param maxdev Maximum deviation of cluster means across time..
#' @param countslist_overwrite
#' @param admm_err_rel
#' @param admm_err_abs
#' @param admm_local_adapt
#' @param admm_local_adapt_niter
#'
#' @return List object with flowtrend model estimates.
#' @export
#'
#' @examples
flowtrend_once <- function(ylist,
                       countslist = NULL,
                       x = NULL,
                       numclust, niter = 1000, l, l_prob = NULL,
                       mn = NULL, lambda = 0, lambda_prob = NULL, verbose = FALSE,
                       tol_em = 1E-4,
                       maxdev = NULL,
                       countslist_overwrite = NULL,
                       ## beta Mstep (ADMM) settings
                       admm = TRUE,
                       admm_err_rel = 1E-3,
                       admm_err_abs = 1E-4,
                       ## Mean M step (Locally Adaptive ADMM) settings
                       admm_local_adapt = TRUE,
                       admm_local_adapt_niter = if(admm_local_adapt) 10 else 1,
                       rho_init = 0.1,
                       ## Other options
                       check_convergence = TRUE,
                       ## Random seed
                       seed = NULL){

  ## Basic checks
  if(!is.null(maxdev)){
    assertthat::assert_that(maxdev!=0)
  } else {
    maxdev = 1E10
  }
  assertthat::assert_that(numclust > 1)
  assertthat::assert_that(niter > 1)
  if(is.null(countslist)){
    ntlist = sapply(ylist, nrow)
    countslist = lapply(ntlist, FUN = function(nt) rep(1, nt))
  }
  if(!is.null(seed)){
    assertthat::assert_that(all((seed %>% sapply(., class)) == "integer"))
    assertthat::assert_that(length(seed) == 7)
  }

  ## Setup for EM algorithm
  TT = length(ylist)
  dimdat = ncol(ylist[[1]])
  if(is.null(x)) x <- 1:TT
  if(is.unsorted(x)) stop("x must be ordered!")

  # l = 2 is quadratic trend filtering
  # l = 1 is linear trend filtering
  # l = 0 is fused lasso
  # D^{(1)} is first differences, so it correponds to l=0
  Dlp1 = gen_diff_mat(n = TT, l = l+1, x = x)
  if(l > 1){
    facmat = diag(l / diff(x, lag = l))
  } else {
    facmat = diag(rep(1, TT-l))
  }
  Dl = facmat %*% gen_diff_mat(n = TT, l = l, x = x)
  tDl = t(Dl)

  Dlsqrd <- t(Dl) %*% Dl
  e_mat <- etilde_mat(TT = TT) # needed to generate B
  Dlp1_prob = gen_diff_mat(n = TT, l = l_prob+1, x = x)
  H_tf <- gen_tf_mat(n = TT, k = l_prob, x = x)
  if(is.null(mn)){
    mn = init_mn(ylist, numclust, TT, dimdat,
                 countslist = countslist, seed = seed)
  }
  ntlist = sapply(ylist, nrow)
  N = sum(ntlist)

  ## Initialize some objects
  prob = matrix(1/numclust, nrow = TT, ncol = numclust) ## Initialize to all 1/K.
  denslist_by_clust <- NULL
  objectives = c(+1E20, rep(NA, niter-1))
  sigma_fac <- diff(range(do.call(rbind, ylist)))/8
  sigma = init_sigma(ylist, numclust, sigma_fac) ## (T x numclust x (dimdat x dimdat))
  sigma_eig_by_clust = NULL
  zero.betas = zero.alphas = list()

  ## The least elegant solution I can think of.. used only for blocked cv
  if(!is.null(countslist_overwrite)) countslist = countslist_overwrite
  #if(!is.null(countslist)) check_trim(ylist, countslist)

  vals <- vector(length = niter)
  latest_rho = NA
  start.time = Sys.time()
  for(iter in 2:niter){
    if(verbose){
      print_progress(iter-1, niter-1, "EM iterations.", start.time = start.time, fill = FALSE)
    }
    resp <- Estep(mn, sigma, prob, ylist = ylist, numclust = numclust,
                  denslist_by_clust = denslist_by_clust,
                  first_iter = (iter == 2), countslist = countslist)

    ## M step (three parts)

    ## 1. Means
    res_mu = Mstep_mu(resp, ylist,
                      lambda = lambda,
                      first_iter = (iter == 2),
                      l = l, Dlp1 = Dlp1, Dl = Dl,
                      tDl = tDl,
                      Dlsqrd = Dlsqrd,
                      sigma_eig_by_clust = sigma_eig_by_clust,
                      sigma = sigma, maxdev = maxdev,
                      e_mat = e_mat,
                      Zs = NULL,
                      Ws = NULL,
                      uws = NULL,
                      uzs =  NULL,
                      x = x,
                      err_rel = admm_err_rel,
                      err_abs = admm_err_abs,
                      local_adapt = admm_local_adapt,
                      local_adapt_niter = admm_local_adapt_niter,
                      rho_init = rho_init,
                      iter = iter)
                      ## rho_init = (if(iter == 2) rho_init else latest_rho))
    ## latest_rho = res_mu$rho
    mn = res_mu$mns

    ## 2. Sigma
    sigma = Mstep_sigma(resp, ylist, mn, numclust)

    ## 3. Probabilities
    prob_link = Mstep_prob(resp, countslist = countslist, H_tf = H_tf,
                           lambda_prob = lambda_prob, l_prob = l_prob, x = x)
    prob = softmax(prob_link)

    objectives[iter] = objective(ylist = ylist, mu = mn, sigma = sigma, prob = prob, prob_link = prob_link,
                                 lambda = lambda, Dlp1 = Dlp1, l = l, countslist = countslist,
                                 Dlp1_prob = Dlp1_prob,
                                 l_prob = l_prob,
                                 lambda_prob = lambda_prob)

    ## Check convergence
    if(iter > 10){ 
      if(check_convergence &
         check_converge_rel(objectives[iter-1], objectives[iter], tol = tol_em) &
         check_converge_rel(objectives[iter-2], objectives[iter-1], tol = tol_em)&
         check_converge_rel(objectives[iter-3], objectives[iter-2], tol = tol_em)){
         ## check_converge_rel(objectives[iter-4], objectives[iter-3], tol = tol_em)){
        break
      }
    }
  }

  return(structure(list(mn = mn,
                        prob = prob,
                        prob_link = prob_link,
                        sigma = sigma,
                        objectives = objectives[2:iter],
                        final.iter = iter,
                        resp = resp,
                        ## Above is output, below are data/algorithm settings.
                        dimdat = dimdat,
                        TT = TT,
                        N = N,
                        l = l,
                        x = x,
                        numclust = numclust,
                        lambda = lambda,
                        lambda_prob = lambda_prob,
                        maxdev = maxdev,
                        niter = niter
  ), class = "flowtrend"))
}
