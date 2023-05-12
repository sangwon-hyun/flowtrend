# Generated from _main.Rmd: do not edit by hand

#' Locally adaptive ADMM.
#'
#' @param K
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
la_admm_oneclust <- function(K,
                             ...){

  ## Initialize arguments for ADMM.
  args <- list(...)
  p = args$p
  l = args$l
  TT = args$TT
  dimdat = args$dimdat

  ## This initialization can come from the previous *EM* iteration.
  if(args$first_iter){
    mu = matrix(0, nrow = TT, ncol = dimdat)
    Z <- matrix(0, nrow = TT, ncol = dimdat)
    W <- matrix(0, nrow = dimdat, ncol = TT - l )
    uz <- matrix(0, nrow = TT, ncol = dimdat)
    uw <- matrix(0, nrow = dimdat, ncol = TT - l )

    #args[['beta']] <- beta
    args[['z']] <- Z
    args[['w']] <- W
    args[['uz']] <- uz
    args[['uw']] <- uw
  }

  cols = c()
  objectives = c()

  ## Temporary
  all_fits = c()
  all_cols = c()

  ## Run ADMM repeatedly with (1) double rho, and (2) previous b
  for(kk in 1:K){
    if(kk > 1){
      Z = matrix(0, nrow = TT, ncol = dimdat)
      W = matrix(0, ncol = TT - l , nrow = dimdat)
      uz = matrix(0, nrow = TT, ncol = dimdat)
      uw = matrix(0, ncol = TT - l , nrow = dimdat)
      args[['mu.warm']] <- mu
      args[['warmstart']] <- T
      args[['z']] <- Z
      args[['w']] <- W
      args[['uz']] <- uz
      args[['uw']] <- uw
      args[['rho']] <- rho
      args[['schurB']][['tQ']] <- tQ
      print(kk)
    }

    ## Run ADMM
    args[['outer_iter']] <- kk
    ## Call main function
    argn <- lapply(names(args), as.name)
    names(argn) <- names(args)
    call <- as.call(c(list(as.name("admm_oneclust")), argn))
    res = eval(call, args)

    ## ## Temporary (uncomment for plotting objectives)
    ## fits = as.numeric(na.omit(res$fits))
    ## all_fits = c(all_fits, fits)
    ## all_cols = c(all_cols, rep(kk, length(fits)))
    ## print(all_fits)
    ## if(length(all_fits)!=0){
    ##   plot(all_fits %>% log10(), col=all_cols, type='o', lwd=2, main = paste0("outer iter = ", kk))
    ## }
    ## ## End temporary

    objectives = c(objectives, res$obj)

    ## Handling the scenario where the objectives are all zero
    padding = 1E-12
    objectives = objectives + padding

    if(args$objective){
      print(objectives)
    }
    ## See if outer iterations should terminate
    if(res$converge | outer_converge(objectives)){
      res$converge <- T
      break
    }

    #browser()

    ## Update some parameters; double the rho value, and update the B matrix
    rho = 2 * args$rho
    tQ = 2 * args$schurB$tQ
    mu = res$mu
    Z = res$Z
    W = res$W
    uz = res$uz
    uw = res$uw
    # browser()
    # print(kk)

  }
  ## if(!res$converge) warning("Didn't converge at all")

  ## Record how long the admm took; in terms of # iterations.
  res$kk = kk

  return(res)
}
