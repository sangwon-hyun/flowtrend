# Generated from _main.Rmd: do not edit by hand

#' Converting to a list of matrices, \code{ylist}, to input to \code{flowsmooth()}.
#'
#' @param dt Output from \code{gendat_1d()}.
#'
#' @return List of matrices
#' @export
dt2ylist <- function(dt){
  dt%>% select(time, Y) %>% arrange(time) %>%
    group_by(time) %>%
    group_split(.keep = FALSE) %>%
    lapply(as.matrix)
}
