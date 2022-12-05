# Generated from _main.Rmd: do not edit by hand

#' Main function for summarizing the cross-validation results.
#'
#' @inheritParams cv_flowsmooth
#' @param save If TRUE, save to \code{file.path(destin, filename)}.
#' @param filename File name to save to.
#'
#' @return List containing summarized results from cross-validation. Here are
#'   some objects in this list: \code{bestres} is the the overall best model
#'   chosen from the cross-validation; \code{cvscoremat} is a 2d matrix of CV
#'   scores from all pairs of regularization parameters; \code{bestreslist} is a
#'   list of all the best models (out of \code{nrestart} EM replications) from the
#'   each pair of lambda values. If \code{isTRUE(save)}, nothing is returned.
#'
#' @export
cv_summary <- function(destin = ".",
                       save = FALSE,
                       filename = "summary.RDS"
                       ){
  ####################
  ## Load data #######
  ####################
  load(file = file.path(destin, 'meta.Rdata'), verbose = FALSE)

  ## This loads all the necessary things: nrestart, nfold, cv_gridsize
  stopifnot(exists("nrestart"))
  stopifnot(exists("nfold"))
  stopifnot(exists("cv_gridsize"))

  ##########################
  ## Get the CV results. ###
  ##########################
  a = cv_aggregate(destin)
  cvscore.mat = a$cvscore.mat
  min.inds = a$min.inds
  

  ## Get results from refitting
  bestreslist = cv_aggregate_res(destin = destin)
  bestres = bestreslist[[paste0(min.inds[1] , "-", min.inds[2])]]
  if(is.null(bestres)){
    stop(paste0("The model with lambda indices (",
                min.inds[1], ",", min.inds[2], ") is not available."))
  }

  ## ########################
  ## ## Get coefficients ####
  ## ########################
  ## betalist =  lapply(1:bestres$numclust, function(iclust){
  ##   ## Get all betas
  ##   rownames(bestres$beta[[iclust]])[-1] = colnames(bestres$X)
  ##   cf = bestres$beta[[iclust]][-1,, drop=FALSE]

  ##   ## Remove the rows that are all zero
  ##   all.zero.rows = which(apply(cf, 1, function(myrow)all(myrow == 0)))
  ##   if(length(all.zero.rows) > 0){
  ##     cf = cf[-all.zero.rows,, drop=FALSE]
  ##   }
  ##   round(Matrix::Matrix(cf, sparse=TRUE),3)
  ## })
  ## names(betalist) = paste0("Beta matrix, cluster ", 1:bestres$numclust)
  ## pretty.betas = betalist
  ## colnames(bestres$alpha)[-1 ] = colnames(bestres$X)
  ## alpha = t(bestres$alpha)
  ## alpha[which(abs(alpha) < 1E-5)] = 0
  ## pretty.alphas = round(Matrix::Matrix(alpha, sparse=TRUE),3)

  ######################
  ## Get the output ####
  ######################
  ## pretty.mns = .
  ## pretty.probs = .
  ## pretty.sigmas = .

  ######################
  ## Get the sigmas ####
  ######################
  if(bestres$dimdat == 1){
    pretty.sigmas = sqrt(bestres$sigma[,1,])
    names(pretty.sigmas) = paste0("Cluster ", 1:bestres$numclust)
  } else {
    sigmas = lapply(1:bestres$numclust, function(iclust){
      diag(bestres$sigma[iclust,,])
    })
    names(sigmas) = paste0("Cluster ", 1:bestres$numclust)
    pretty.sigmas = lapply(sigmas, sqrt)
  }

  out = list(bestres = bestres,
             cvscore.mat = cvscore.mat,
             min.inds = min.inds,
             ## ## Pretty formatted data ## Todo: get this done.
             ## pretty.mns = pretty.mns,
             ## pretty.probs = pretty.probs,
             ## pretty.sigmas = pretty.sigmas,
             ## List of all best models for all lambda pairs.
             bestreslist = bestreslist,
             destin = destin)

  if(save){ saveRDS(out, file=file.path(destin, filename))}
  return(out)
}
