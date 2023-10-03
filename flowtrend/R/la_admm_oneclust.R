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
la_admm_oneclust <- function(K, ...){

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
    W <- matrix(0, nrow = TT-l, ncol = dimdat)
    uz <- matrix(0, nrow = TT, ncol = dimdat)
    uw <- matrix(0, nrow = TT - l, ncol = dimdat)

    #args[['beta']] <- beta
    args[['mu']] <- mu
    args[['z']] <- Z
    args[['w']] <- W
    args[['uz']] <- uz
    args[['uw']] <- uw
  }

  cols = c()
  all_admm_objectives = c()
  some_admm_objectives = c()

  ## Temporary
  all_fits = c()
  all_cols = c()

  ## Run ADMM repeatedly with (1) double rho, and (2) previous b
  for(kk in 1:K){
    cat("cluster kk="); cat(kk); cat(fill=TRUE)
    cat("rho="); 
    if(kk == 1){
      ## cat(rho); cat(fill=TRUE)
      cat(args[['rhoinit']]); cat(fill=TRUE)
      browser()
    }
    if(kk > 1){
      print(rho)
      ## Z = matrix(0, nrow = TT, ncol = dimdat)
      ## W = matrix(0, nrow = TT - l , ncol = dimdat)
      ## uz = matrix(0, nrow = TT, ncol = dimdat)
      ## uw = matrix(0, nrow = TT - l , ncol = dimdat)

      ## These ensure warm starts are true
      print("head(mu)")
      print(head(mu))
      args[['mu']] <- mu
      args[['z']] <- Z
      args[['w']] <- W
      args[['uz']] <- uz
      args[['uw']] <- uw
      args[['rho']] <- rho
      ## args[['schurB']][['tQ']] <- tQ ## Why do we need this?
    }

    ## Run ADMM
    args[['outer_iter']] <- kk

    ## Call main function
    argn <- lapply(names(args), as.name)
    names(argn) <- names(args)
    call <- as.call(c(list(as.name("admm_oneclust")), argn))
    res = eval(call, args)

    ## Temporary (uncomment for plotting objectives)
    ## fits = as.numeric(na.omit(res$fits))
    ## all_fits = c(all_fits, fits)
    ## all_cols = c(all_cols, rep(kk, length(fits)))
    ## print(all_fits)
    ## if(length(all_fits)!=0){
    ##   plot(all_fits %>% log10(), col=all_cols, type='o', lwd=2, main = paste0("outer iter = ", kk))
    ## }
    ## ## End temporary
    all_admm_objectives = c(all_admm_objectives, res$admm_objectives)
    some_admm_objectives = c(some_admm_objectives, res$single_admm_objective)

    if(FALSE){
    if(kk==4){pdf(paste0("~/Downloads", Sys.time(), "-", kk, ".pdf"), width=5, height = 5)}
    plot(all_admm_objectives, type = 'l')
    if(kk==4){dev.off()}
    }

    ## Handling the scenario where the objectives are all zero
    padding = 1E-12
    some_admm_objectives = some_admm_objectives + padding

    ## See if outer iterations should terminate
    if(res$converge | outer_converge(some_admm_objectives)){
      res$converge <- T
      break
    }

    ## Update some parameters; double the rho value, and update the B matrix
    rho = 2 * args$rho
    tQ = 2 * args$schurB$tQ
    mu = res$mu
    Z = res$Z
    W = res$W
    uz = res$uz
    uw = res$uw
  }
  if(!res$converge) warning("ADMM didn't converge for one cluster.")

  ## Record how long the admm took; in terms of # iterations.
  res$kk = kk

  return(res)
}
