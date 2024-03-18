# Generated from _main.Rmd: do not edit by hand

#' Helper function to run ONE job for CV, in iprob, imu, ifold, irestart.
#'
#' @param iprob Index for prob.
#' @param imu Index for beta.
#' @param ifold Index for CV folds.
#' @param irestart Index for 1 through nrestart.
#' @param folds CV folds (from \code{make_cv_folds()}).
#' @param destin Destination directory.
#' @param lambda_means List of regularization parameters for mean model.
#' @param lambda_probs List of regularization parameters for prob model.
#' @param ylist Data.
#' @param countslist Counts or biomass.
#' @param ... Rest of arguments for \code{flowtrend_once()}.
#'
#' @return Nothing is returned. Instead, a file named "1-1-1-1-cvscore.Rdata"
#'   is saved in \code{destin}. (The indices here are iprob-imu-ifold-irestart).
#'
#' @export
one_job <- function(iprob, imu, ifold, irestart, folds, destin,
                       lambda_means, lambda_probs,
                       seedtab = NULL,
                       ## The rest that is needed explicitly for flowtrend()
                       ylist, countslist,
                       l, l_prob,
                       ...){

  ## Get the train/test data
  TT <- length(ylist)
  test.inds = unlist(folds[ifold]) %>% sort()
  test.dat = ylist[test.inds]
  test.count = countslist[test.inds]
  train.inds = c(1, unlist(folds[-ifold]), TT) %>% sort()
  train.dat = ylist[train.inds]
  train.count = countslist[train.inds]

  ## Check whether this job has been done already.
  filename = make_cvscore_filename(iprob, imu, ifold, irestart)
  best_filename = make_best_cvscore_filename(iprob, imu, ifold)
  ## if(file.exists(file.path(destin, filename)) ){
  if(file.exists(file.path(destin, filename)) | file.exists(file.path(destin, best_filename)) ){
    cat(fill=TRUE)
    cat(filename, "already done.", fill=TRUE)
    return(NULL)
  }

  ## Get the seed ready
  if(!is.null(seedtab)){
    seed = seedtab %>%
      dplyr::filter(iprob == !!iprob,
                    imu == !!imu,
                    ifold == !!ifold,
                    irestart == !!irestart) %>%
      dplyr::select(seed1, seed2, seed3, seed4, seed5, seed6, seed7) %>% unlist() %>% as.integer()
  } else {
    seed = NULL
  }


  lambda_prob = lambda_probs[iprob]
  lambda_mean = lambda_means[imu]

  ## Run the algorithm (all this trouble because of |nrestart|)
  args = list(...)
  args$ylist = train.dat
  args$countslist = train.count
  args$x = train.inds
  args$lambda = lambda_mean
  args$lambda_prob = lambda_prob
  args$l = l
  args$l_prob = l_prob
  args$seed = seed
  if("nrestart" %in% names(args)){
    args = args[-which(names(args) %in% "nrestart")] ## remove |nrestart| prior to feeding to flowtrend_once().
  }

  tryCatch({

    ## Estimate model
    argn <- lapply(names(args), as.name)
    names(argn) <- names(args)
    call <- as.call(c(list(as.name("flowtrend_once")), argn))
    res.train = eval(call, args)

    ## Assign mn and prob
    pred = predict_flowtrend(res.train, newtimes = test.inds)
    stopifnot(all(pred$prob >= 0))

    ## Build Dl
    ##

    ## Evaluate on test data, by calculating objective (penalized likelihood with penalty parameters set to 0)
    cvscore = objective(mu = pred$mn,
                        prob = pred$prob,
                        sigma = pred$sigma,
                        ylist = test.dat,
                        countslist = test.count,
                        unpenalized = TRUE)
                        ## Dl = diag(rep(1, length(test.count))), ## TODO: what is wrong here?
                        ## lambda_prob = 0,
                        ## lambda = 0)
                        ## prob = res.train$prob,
                        ## beta = res.train$beta)

    ## Store (temporarily) the run times
    time_per_iter = res.train$time_per_iter
    final_iter = res.train$final.iter
    total_time = res.train$total_time

    ## Store the results.
    mn = res.train$mn
    prob = res.train$prob
    objectives = res.train$objectives

    ## Save the CV results
    save(cvscore,
         ## Time
         time_per_iter,
         final_iter,
         total_time,
         ## Results
         lambda_mean,
         lambda_prob,
         lambda_means,
         lambda_probs,
         mn,
         prob,
         objectives,
         ## Save the file
         file = file.path(destin, filename))
    return(NULL)

  }, error = function(err) {
    err$message = paste(err$message,
                        "\n(No file will be saved for lambdas (",
                        signif(lambda_probs[iprob],3), ", ", signif(lambda_means[imu],3),
                        ") whose indices are: ",
                        iprob, "-", imu, "-", ifold, "-", irestart,
                        " .)",sep="")
    cat(err$message, fill=TRUE)
    warning(err)})
}
