# Generated from _main.Rmd: do not edit by hand

#' Cross-validation for flowtrend(). Saves results to separate files in
#' \code{destin}.
#'
#' @param destin Directory where output files are saved.
#' @param nfold Number of cross-validation folds. Defaults to 5.
#' @param nrestart Number of repetitions.
#' @param save_meta If TRUE, save meta data.
#' @param lambda_means Regularization parameters for means.
#' @param lambda_probs Regularization parameters for probs.
#' @param folds Manually provide CV folds (list of time points of data to use
#'   as CV folds). Defaults to NULL.
#' @param mc.cores Use this many CPU cores.
#' @param blocksize Contiguous time blocks from which to form CV time folds.
#' @param refit If TRUE, estimate the model on the full data, for each pair of
#'   regularization parameters.
#' @param ... Additional arguments to flowtrend().
#' @inheritParams flowtrend_once
#'
#' @return No return.
#'
#' @export
cv_flowtrend <- function(## Data
                         ylist,
                         countslist,
                         x = NULL, ## THIS IS NEW
                         ## Define the locations to save the CV.
                         destin = ".",
                         ## Regularization parameter values
                         lambda_means,
                         lambda_probs,
                         l,
                         l_prob,
                         iimat = NULL,
                         ## Other settings
                         maxdev,
                         numclust,
                         nfold,
                         blocksize,
                         nrestart,
                         verbose = FALSE,
                         refit = FALSE,
                         save_meta = FALSE,
                         mc.cores = 1,
                         folds = NULL,
                         seedtab = NULL,
                         niter = 1000,
                         ...){

  ## Basic checks
  stopifnot(length(lambda_probs) == length(lambda_means))
  cv_gridsize = length(lambda_means)

  ## There's an option to input one's own iimat matrix.
  if(is.null(iimat)){
    ## Make an index of all jobs
    if(!refit) iimat = make_iimat(cv_gridsize, nfold, nrestart)
    if(refit) iimat = make_iimat_small(cv_gridsize, nrestart)
  }

  ## Define the CV folds
  ## folds = make_cv_folds(ylist = ylist, nfold = nfold, blocksize = 1)
  if(is.null(folds)){
    folds = make_cv_folds(ylist = ylist, nfold = nfold, blocksize = blocksize)
  } else {
    stopifnot(length(folds) == nfold)
  }

  ## Save meta information, once.
  if(save_meta){
    ##if(!refit){
      if(file.exists(file = file.path(destin, 'meta.Rdata'))){

        ## Put aside the current guys
        cat(fill = TRUE)
        cat("Meta data already exists!")
        folds_current = folds
        nfold_current = nfold
        nrestart_current = nrestart
        cv_gridsize_current = cv_gridsize
        lambda_means_current = lambda_means
        lambda_probs_current = lambda_probs
        ylist_current = ylist
        x_current = x
        countslist_current = countslist

        ## Load the saved metadata and check if they are all the same as the current guys
        load(file = file.path(destin, 'meta.Rdata'), verbose = FALSE)
        stopifnot(identical(folds, folds_current))
        stopifnot(nfold == nfold_current)
        stopifnot(nrestart == nrestart_current) ## Added recently
        stopifnot(cv_gridsize == cv_gridsize_current)
        stopifnot(all(lambda_means == lambda_means_current))
        stopifnot(all(lambda_probs == lambda_probs_current))
        stopifnot(identical(ylist, ylist_current))
        stopifnot(identical(x, x_current))
        stopifnot(identical(countslist, countslist_current))
        cat(fill=TRUE)
        cat("Successfully checked that the saved metadata is identical to the current one.", fill = TRUE)
      } else {
        save(folds,
             nfold,
             nrestart, ## Added recently
             cv_gridsize,
             lambda_means,
             lambda_probs,
             ylist, countslist,
             x,
             ## Save the file
             file = file.path(destin, 'meta.Rdata'))
        print(paste0("wrote meta data to ", file.path(destin, 'meta.Rdata')))
      }
    ## }
  }

  ## Run the EM algorithm many times, for each value of (iprob, imu, ifold, irestart)
  start.time = Sys.time()
  parallel::mclapply(1:nrow(iimat), function(ii){
    print_progress(ii, nrow(iimat), "Jobs (EM replicates) assigned on this computer", start.time = start.time)

    if(!refit){
      iprob = iimat[ii,"iprob"]
      imu = iimat[ii,"imu"]
      ifold = iimat[ii,"ifold"]
      irestart = iimat[ii,"irestart"]
      ## if(verbose) cat('(iprob, imu, ifold, irestart)=', c(iprob, imu, ifold, irestart), fill=TRUE)
    } else {
      iprob = iimat[ii, "iprob"]
      imu = iimat[ii, "imu"]
      ifold = 0
    }

    if(!refit){
      one_job(iprob = iprob,
              imu = imu,
              l = l,
              l_prob = l_prob,
              ifold = ifold,
              irestart = irestart,
              folds = folds,
              destin = destin,
              lambda_means = lambda_means,
              lambda_probs = lambda_probs,
              ## Arguments for flowtrend()
              ylist = ylist, countslist = countslist,
              x = x,
              ## Additional arguments for flowtrend().
              numclust = numclust,
              maxdev = maxdev,
              verbose = FALSE,
              seedtab = seedtab,
              niter = niter)
    } else {
      one_job_refit(iprob = iprob,
                    imu = imu,
                    l = l,
                    l_prob = l_prob,
                    destin = destin,
                    lambda_means = lambda_means,
                    lambda_probs = lambda_probs,
                    ## Arguments to flowtrend()
                    ylist = ylist, countslist = countslist,
                    x = x,
                    ## Additional arguments for flowtrend().
                    numclust = numclust,
                    maxdev = maxdev,
                    nrestart = nrestart,
                    verbose = FALSE,
                    seedtab = seedtab,
                    niter = niter)
    }
    return(NULL)
  }, mc.cores = mc.cores)
}
