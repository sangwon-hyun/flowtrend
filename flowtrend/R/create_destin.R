# Generated from _main.Rmd: do not edit by hand

#' Creates a directory \code{destin}, if it doesn't already exist.
#'
#' @param destin Destination directory.
#'
#' @return Nothing.
#' @export
create_destin <- function(destin){
  if(!dir.exists(destin)){
    dir.create(destin, recursive = TRUE)
    cat("Creating destin: ", destin, fill=TRUE)
  } else {
    cat("All output goes out to destin: ", destin, fill = TRUE)
  }
}
