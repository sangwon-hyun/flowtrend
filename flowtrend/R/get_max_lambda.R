# Generated from _main.Rmd: do not edit by hand

#' A wrapper for \code{calc_max_lambda}. Saves the two maximum lambda values in
#' a file.
#'
#' @param destin Where to save the output (A two-lengthed list called
#'   "maxres").
#' @param maxres_file Filename for output. Defaults to maxres.Rdata.
#' @param ... Additional arguments to \code{flowtrend()}.
#' @inheritParams calc_max_lambda
#'
#' @return No return
#'
#' @export
get_max_lambda <- function(destin, maxres_file = "maxres.Rdata",
                           ylist,
                           countslist,
                           numclust,
                           maxdev,
                           max_lambda_mean,
                           max_lambda_prob,
                           ...){
  
  if(file.exists(file.path(destin, maxres_file))){
    load(file.path(destin, maxres_file))
    cat("Maximum regularization values are loaded.", fill=TRUE)
    return(maxres)
  } else {
    print(Sys.time())
    cat("Maximum regularization values being calculated.", fill = TRUE)
    cat("with initial lambda values (prob and mu):", fill = TRUE)
    print(c(max_lambda_prob, max_lambda_mean));
    maxres = calc_max_lambda(ylist = ylist,
                             countslist = countslist,
                             numclust = numclust,
                             maxdev = maxdev,
                             ## This function's settings
                             max_lambda_prob = max_lambda_prob,
                             max_lambda_mean = max_lambda_mean,
                             ...)
    print(maxres)
    save(maxres, file = file.path(destin, maxres_file))
    cat("file was written to ", file.path(destin, maxres_file), fill=TRUE)
    cat("maximum regularization value calculation done.", fill = TRUE)
    print(Sys.time())
    return(maxres)
  }
}
