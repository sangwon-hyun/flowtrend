# Generated from _main.Rmd: do not edit by hand

#' Computes the M step for mu. TODO: use templates for the argument. As shown
#' here:
#' https://stackoverflow.com/questions/15100129/using-roxygen2-template-tags
#' 
#' @param resp Responsbilities of each particle.
#' @param ylist 
#' @param lambda
#' @param l
#' @param sigma
#' @param sigma_eig_by_clust
#' @param Dl
#' @param Dlp1
#' @param TT
#' @param N
#' @param dimdat
#' @param first_iter
#' @param mus
#' @param Zs
#' @param Ws
#' @param uws
#' @param uzs
#' @param maxdev
#' @param x
#' @param niter
#' @param err_rel
#' @param err_abs
#' @param zerothresh
#' @param local_adapt
#' @param local_adapt_niter
#' @param space
#'
#' @return
#' @export
#'
#' @examples
Mstep_mu <- function(resp,
                     ylist,
                     lambda = 0.5,
                     l = 3,
                     sigma,
                     sigma_eig_by_clust = NULL,
                     Dlsqrd,
                     Dl, Dlp1,  TT, N, dimdat,
                     first_iter = TRUE,
                     e_mat,

                     ## Warm startable variables
                     mus = NULL,
                     Zs = NULL,
                     Ws = NULL,
                     uws = NULL,
                     uzs = NULL,
                     ## End of warm startable variables

                     maxdev = NULL,
                     x = NULL,
                     niter = (if(local_adapt) 1e2 else 1e4),
                     err_rel = 1E-3,
                     err_abs = 0,
                     zerothresh = 1E-6,
                     local_adapt = FALSE,
                     local_adapt_niter = 10,
                     space = 50){

  ####################
  ## Preliminaries ###
  ####################
  TT = length(ylist)
  numclust = ncol(resp[[1]])
  dimdat = ncol(ylist[[1]])
  ntlist = sapply(ylist, nrow)
  resp.sum = lapply(resp, colSums) %>% do.call(rbind, .)
  N = sum(unlist(resp.sum)) ## NEW (TODO: make more efficient, later)

  # starting rho for LA-ADMM
  if(local_adapt){
    rho.init = .001
  } else{
    if(!is.null(x)){
      rho.init = lambda*((max(x) - min(x))/length(x))^l ## Where did this come from?
      rho.init = lambda ## Which, of the two, should one use? 
    } else {
      rho.init = lambda
    }
  } 


  ## Other preliminaries
  schur_syl_A_by_clust = schur_syl_B_by_clust = term3list = list()
  ybarlist = list()
  ycentered_list = Xcentered_list = yXcentered_list = list()
  Qlist = list()
  sigmainv_list = list()
  convergences = list()
  for(iclust in 1:numclust){

    ## Retrieve sigma inverse from pre-computed SVD, if necessary
    if(is.null(sigma_eig_by_clust)){
      sigmainv = solve(sigma[iclust,,])
    } else {
      sigmainv = sigma_eig_by_clust[[iclust]]$sigma_inv
    }

    resp.iclust <- lapply(resp, FUN = function(r) matrix(r[,iclust]))

    ## Center y and X
    # obj <- weight_ylist(iclust, resp, resp.sum, ylist)
    #ycentered <- obj$ycentered

    ## Form the Sylvester equation coefficients in AX + XB + C = 0
    # syl_A = rho * sigma[iclust,,]
    # Q = 1/N * t(Xcentered) %*% D %*% Xcentered
    # syl_B = Q %*% Xinv

    AB <- get_AB_mats(y = y, resp = resp.iclust, Sigma_inv = sigmainv,
                      e_mat = e_mat, N = N, Dlp1 = Dlp1, Dl = Dl,
                      Dlsqrd = Dlsqrd, rho = rho.init, z = NULL, w = NULL,
                      uz = NULL, uw = NULL)

    ## Store the Schur decomposition
    schur_syl_A_by_clust[[iclust]] = myschur(AB$A)
    schur_syl_B_by_clust[[iclust]] = myschur(AB$B)

    ## Calculate coefficients for objective value  calculation
    # Qlist[[iclust]] = Q

    ## ## Also calculate some things for the objective value
    ## ylong = sweep(do.call(rbind, ylist), 2, obj$ybar)
    ## longwt = do.call(c, lapply(1:TT, function(tt){ resp[[tt]][,iclust]})) %>% sqrt()
    ## wt.long = longwt * ylong
    ## wt.ylong = longwt * ylong
    ## crossprod(wt.ylong, wt.ylong)

    ## Store the third term
    # term3list[[iclust]] =  1 / N * sigmainv %*% yXcentered
    # ybarlist[[iclust]] = obj$ybar
    #
    ycentered <- NULL
     ycentered_list[[iclust]] = ycentered
     #print(ycentered)
    # Xcentered_list[[iclust]] = Xcentered
    # yXcentered_list[[iclust]] = yXcentered
    sigmainv_list[[iclust]] = sigmainv
  }

  ##########################################
  ## Run ADMM separately on each cluster ##
  #########################################
  admm_niters = admm_inner_iters = vector(length = numclust, mode = "list")
  if(first_iter) mus = vector(length = numclust, mode = "list")
 # if(first_iter){
    Zs <- lapply(1:numclust, function(x) matrix(0, nrow = TT, ncol = dimdat))
    Ws <- lapply(1:numclust, function(x) matrix(0, nrow = TT - l, ncol = dimdat))
    uzs <- lapply(1:numclust, function(x) matrix(0, nrow = TT, ncol = dimdat))
    uws <- lapply(1:numclust, function(x) matrix(0, nrow = TT - l, ncol = dimdat))

   # Zs =  Ws =  Us  = vector(length = numclust, mode = "list")
 # }

  fits = matrix(NA, ncol = numclust, nrow = ceiling(niter / space))

  ## For every cluster, run LA-ADMM
  resid_mat_list = list()
  start.time = Sys.time()
  for(iclust in 1:numclust){
    resp.iclust <- lapply(resp, FUN = function(r) matrix(r[,iclust]))
    resp.sum.iclust <- lapply(resp.sum, FUN = function(r) matrix(r[iclust]))

    ## Possibly locally adaptive ADMM, for now just running with rho == lambda
    res = la_admm_oneclust(K = (if(local_adapt) local_adapt_niter else 1),
                           local_adapt = local_adapt,
                           iclust = iclust,
                           niter = niter,
                           TT = TT, N = N, dimdat = dimdat, maxdev = maxdev,
                           schurA = schur_syl_A_by_clust[[iclust]],
                           schurB = schur_syl_B_by_clust[[iclust]],
                           #term3 = term3list[[iclust]],
                           sigmainv = sigmainv_list[[iclust]],
                           # Xinv = Xinv,
                           # Xaug = Xaug,
                           # Xa = Xa,
                           rho = rho.init,
                           rhoinit = rho.init,
                           sigma = sigma,
                           lambda = lambda,
                           resp = resp.iclust,
                           l = l,
                           Dlp1 = Dlp1,
                           Dl = Dl,
                          #resp.sum = resp.sum.iclust,
                           y = ylist,
                           err_rel = err_rel,
                           err_abs = err_abs,
                           zerothresh = zerothresh,
                           sigma_eig_by_clust = sigma_eig_by_clust,
                           space = space,

                           ## Warm starts from previous *EM* iteration
                           first_iter = first_iter,
                           ## mu = mus[[iclust]], ## I think we want this.
                           uw = uws[[iclust]],
                           uz = uzs[[iclust]],
                           z = Zs[[iclust]],
                           w = Ws[[iclust]]
    )

    ## Store the results
    mus[[iclust]] = res$mu
    ## fits[,iclust] = res$fits ## TODO: revive this, for testing? We'll see.
    admm_niters[[iclust]] = res$kk
    admm_inner_iters[[iclust]] = res$inner.iter

    ## Store other things for for warmstart
    ## mus[[iclust]] = res$mu
    Zs[[iclust]] = res$Z
    uzs[[iclust]] = res$uz
    uws[[iclust]] = res$uw
    Ws[[iclust]] = res$W
    ## The upper triangular matrix remains the same. (code missing?)

    resid_mat_list[[iclust]] = res$resid_mat ## temporary
    convergences[[iclust]] = res$converge
   # print(res$converge)
  }

  ## Aggregate the yhats into one array
  mu_array = array(NA, dim = c(TT, dimdat, numclust))
  for(iclust in 1:numclust){ mu_array[,,iclust] = mus[[iclust]] }

  ## Each are lists of length |numclust|.
  return(list(mns = mu_array,
              ## fits = fits,
              resid_mat_list = resid_mat_list,
              convergences = convergences,
              admm_niters = admm_niters, ## Temporary: Seeing the number of
              ## outer iterations it took to
              ## converge.
              admm_inner_iters = admm_inner_iters,

              ## For warmstarts
              Zs = Zs,
              Ws = Ws,
              uws = uws,
              uzs = uzs,
              N = N,

              ## For using in the Sigma M step
              ycentered_list = ycentered_list,
              Xcentered_list = Xcentered_list,
              yXcentered_list = yXcentered_list,
              Qlist = Qlist
  ))
}
