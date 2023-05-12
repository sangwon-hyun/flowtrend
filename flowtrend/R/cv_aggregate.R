# Generated from _main.Rmd: do not edit by hand

#' Aggregate CV scores from the results, saved in \code{destin}.
#'
#' @param destin Directory with cross-validation output.
#'
#' @export
cv_aggregate <- function(destin){

  ## ## Read the meta data (for |nfold|, |cv_gridsize|, |nrestart|, |lambda_means|,
  ## ## |lambda_probs|)
  load(file = file.path(destin, 'meta.Rdata'), verbose = FALSE)
  
  ## This loads all the necessary things; just double-checking.
  stopifnot(exists("nrestart"))
  stopifnot(exists("nfold"))
  stopifnot(exists("cv_gridsize"))
  stopifnot(exists(c("lambda_probs")))
  stopifnot(exists(c("lambda_means")))

  ## Aggregate the results
  cvscore.array = array(NA, dim = c(cv_gridsize, cv_gridsize, nfold, nrestart))
  cvscore.mat = matrix(NA, nrow = cv_gridsize, ncol = cv_gridsize)
  for(iprob in 1:cv_gridsize){
    for(imu in 1:cv_gridsize){
      obj = matrix(NA, nrow=nfold, ncol=nrestart)
      for(ifold in 1:nfold){
        for(irestart in 1:nrestart){
          filename = make_cvscore_filename(iprob, imu, ifold, irestart)
          tryCatch({
            load(file.path(destin, filename), verbose = FALSE)
            cvscore.array[iprob, imu, ifold, irestart] = cvscore
            obj[ifold, irestart] = objectives[length(objectives)]
          }, error = function(e){})
        }
      }

      ## Pick out the CV scores with the *best* (lowest) objective value
      cvscores = cvscore.array[iprob, imu , ,]
      best.models = apply(obj, 1, function(myrow){
        ind = which(myrow == min(myrow, na.rm=TRUE))
        if(length(ind)>1) ind = ind[1]  ## Just choose one, if there is a tie.
        return(ind)
      })
      final.cvscores = sapply(1:nfold, function(ifold){
        #cvscores[ifold, best.models[ifold]]
        cvscores[ifold]
      })
      cvscore.mat[iprob, imu] = mean(final.cvscores)
    }
  }

  ## Clean a bit
  cvscore.mat[which(is.nan(cvscore.mat), arr.ind=TRUE)] = NA

  ## ## Read the meta data (for |nfold|, |cv_gridsize|, |nrestart|)
  rownames(cvscore.mat) = signif(lambda_probs,3)
  colnames(cvscore.mat) = signif(lambda_means,3)


  ## Find the minimum
  mat = cvscore.mat
  min.inds = which(mat == min(mat, na.rm = TRUE), arr.ind = TRUE)

  ## Return the results
  out = list(cvscore.array = cvscore.array,
              cvscore.mat = cvscore.mat,
              lambda_means = lambda_means,
              lambda_probs = lambda_probs,
              min.inds = min.inds)
  return(out)
}


#' Helper to aggregate parallelized CV results and obtain the |res| object, all
#' saved in |destin|.
#'
#' @inheritParams cv_aggregate
#'
#' @return List containing, for every (iprob, imu), the "best" estimated
#'   model out of the |nrestart| replicates (best in the sense that it had the best
#'   likelihood value out of the |nrestart| replicates.)
cv_aggregate_res <- function(destin){

  load(file.path(destin, "meta.Rdata"))

  ## df.mat = matrix(NA, ncol=cv_gridsize, nrow=cv_gridsize)
  res.list = list()
  for(iprob in 1:cv_gridsize){
    for(imu in 1:cv_gridsize){

      ## Objective values, over nrestart
      obj = rep(NA, nrestart)
      ## df = rep(NA, nrestart) #
      res.list.inner = list()
      for(irestart in 1:nrestart){
        filename = make_refit_filename(iprob, imu, irestart)
        tryCatch({
          ## Load fitted result
          load(file.path(destin, filename))
          res.list.inner[[irestart]] = res

          ## Also store objective
          obj[irestart] = res$objectives[length(res$objectives)]

        }, error = function(e){})
      }

      ## Calculate the df of the best model
      if(!all(is.na(obj))){
        res.list[[paste0(iprob, "-", imu)]] = res.list.inner[[which.min(obj)]] ## which.min?
      }
    }
  }
  return(res.list)
}
