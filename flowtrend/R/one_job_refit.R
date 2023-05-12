# Generated from _main.Rmd: do not edit by hand

#' Refit model for one pair of regularization parameter values. Saves to
#' \code{nrestart} files named like "1-4-3-fit.Rdata", for
#' "(iprob)-(imu)-(irestart)-fit.Rdata".
#'
#' (Note, \code{nrestart} is not an input to this function.)
#'
#' @inheritParams one_job
#'
#' @export
one_job_refit <- function(iprob, imu, destin,
                          lambda_means, lambda_probs,
                          l, l_prob,
                          seedtab = NULL,
                          ## The rest that is needed explicitly for flowtrend_once()
                          ylist, countslist,
                          ...){

  args = list(...)
  nrestart = args$nrestart
  assertthat::assert_that(!is.null(nrestart))
  for(irestart in 1:nrestart){

    ## Writing file
    filename = make_refit_filename(iprob = iprob, imu = imu, irestart = irestart)
    if(file.exists(file.path(destin, filename))){
      cat(filename, "already done", fill=TRUE)
      next
    } else {

     ## Get the seed ready
     if(!is.null(seedtab)){
       ifold = 0
       seed = seedtab %>%
         dplyr::filter(iprob == !!iprob,
                       imu == !!imu,
                       ifold == !!ifold,
                       irestart == !!irestart) %>%
         dplyr::select(seed1, seed2, seed3, seed4, seed5, seed6, seed7) %>% unlist() %>% as.integer()
     } else {
       seed = NULL
     }

      ## Get the fitted results on the entire data
      args = list(...)
      args$ylist = ylist
      args$countslist = countslist
      args$lambda_prob = lambda_probs[iprob]
      args$lambda = lambda_means[imu]
      args$l = l
      args$l_prob = l_prob
      args$seed = seed
      if("nrestart" %in% names(args)) args = args[-which(names(args) %in% "nrestart")] ## remove |nrestart| prior to feeding

      ## Call the function.
      argn <- lapply(names(args), as.name)
      names(argn) <- names(args)
      call <- as.call(c(list(as.name("flowtrend_once")), argn))
      res = eval(call, args)

      ## Save the results
      cat("Saving file here:", file.path(destin, filename), fill=TRUE)
      save(res, file=file.path(destin, filename))
    }
  }
}
