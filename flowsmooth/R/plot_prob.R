# Generated from _main.Rmd: do not edit by hand

#' Makes cluster probability plot (lines over time).
#'
#' @param obj Estimated model (from e.g. \code{flowsmooth()})
#'
#' @export
plot_prob <- function(obj, x = NULL){

  ## Basic checks
  if(!is.null(x)){
    stopifnot(length(x) == length(ylist))
    times = x
  } else {
    times = 1:length(ylist)
  }

  numclust = obj$numclust
  probmat = obj$prob %>% as_tibble() %>% setNames(1:numclust) %>% add_column(time = times)
  prob_long = probmat %>% pivot_longer(-time, names_to = "cluster", values_to = "prob")
  prob_long %>% ggplot() +
    geom_line(aes(x=time, y = prob, group = cluster, col = cluster)) +
    geom_point(aes(x=time, y = prob, group = cluster, col = cluster)) +
    xlab("Estimated cluster probability")
}
