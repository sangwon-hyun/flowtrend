# Generated from _main.Rmd: do not edit by hand

#' Combine list of ggplots
#' @export
my_mfrow <- function(glist, ncol = NULL, nrow = NULL){
  if(is.null(ncol)) ncol = 1
  if(is.null(nrow)) ncol = 3
  do.call(ggpubr::ggarrange, c(glist, ncol=ncol, nrow=nrow))
}
