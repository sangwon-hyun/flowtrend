# Generated from _main.Rmd: do not edit by hand

#' Helper to divide up the jobs in \code{iimat} into a total of
#' \code{arraynum_max} jobs. The purpose is to divide the jobs, in order to run
#' this on a server.
#'
#' @param arraynum_max Maximum SLURM array number.
#' @param iimat matrix whose rows contain CV job indices.
#'
#' @export
make_iilist <- function(arraynum_max, iimat){
  iimax = nrow(iimat)
  if(arraynum_max > iimax){
    iilist = lapply(1:iimax, function(a)a)
  } else {
    ends = round(seq(from=0, to=iimax, length=arraynum_max+1))
    iilist = Map(function(a,b){ (a+1):b}, ends[-length(ends)], ends[-1])
    stopifnot(length(unlist(iilist)) == nrow(iimat))
  }
  stopifnot(length(unlist(iilist)) == nrow(iimat))
  stopifnot(all(sort(unique(unlist(iilist))) == sort(unlist(iilist))))
  return(iilist)
}
