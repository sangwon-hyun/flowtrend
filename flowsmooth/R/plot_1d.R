# Generated from _main.Rmd: do not edit by hand

#' Makes 1d plot of data and model
#'
#' @param ylist Data.
#' @param obj flowsmooth object. Defaults to NULL.
#' @param x time points. Defaults to NULL.
#'
#' @return ggplot object with data, and optionally, a flowsmooth model overlaid.
#' @export
plot_1d <- function(ylist, obj=NULL, x = NULL){

  ## Basic checks
  if(!is.null(x)){
    stopifnot(length(x) == length(ylist))
    times = x
  } else {
    times = 1:length(ylist)
  }

  ## make data into long matrix
  ymat <- lapply(1:length(ylist), FUN = function(tt){
    data.frame(time = times[tt], Y = ylist[[tt]])
  }) %>% bind_rows() %>% as_tibble()

  ## plot long matrix
  gg = ymat %>% ggplot() +
    geom_point(aes(x = time, y = Y), alpha = .1) +
    theme_bw() + ylab("Data") + xlab("Time") 
    ## theme(legend.position = 'none')
  if(is.null(obj)) return(gg)

  ## Add the model
  numclust = obj$numclust
  mnmat = obj$mn %>% .[,1,] %>% as_tibble() %>% setNames(1:numclust) %>% add_column(time = times)
  probmat = obj$prob %>% as_tibble() %>% setNames(1:numclust) %>% add_column(time = times)
  mn_long = mnmat %>% pivot_longer(-time, names_to = "cluster", values_to = "mean")
  prob_long = probmat %>% pivot_longer(-time, names_to = "cluster", values_to = "prob")
  est_long = full_join(mn_long, prob_long)
  gg = gg + geom_line(aes(x = time, y = mean, size = prob, group = cluster, color = cluster),
                      data = est_long) +
    geom_point(aes(x = time, y = mean, size = prob, group = cluster),
              data = est_long, size = rel(1), shape = 17)

  ## TODO: make it ignore the missing values at the gaps; currently this is not coded as NAs.

  ## Add the estimated 95% probability regions for data.
  stdev = obj$sigma %>% .[,,1] %>% sqrt()
  band_long =
    mn_long %>% mutate(upper = case_when(cluster == "1" ~ mean + 1.96 * stdev[1],
                                         cluster == "2" ~ mean + 1.96 * stdev[2],
                                         cluster == "3" ~ mean + 1.96 * stdev[3]),
                       lower = case_when(cluster == "1" ~ mean - 1.96 * stdev[1],
                                         cluster == "2" ~ mean - 1.96 * stdev[2],
                                         cluster == "3" ~ mean - 1.96 * stdev[3]))
  gg + geom_line(aes(x = time, y = upper, group = cluster, color = cluster),
                 data = band_long, size = rel(1), alpha = .5) +
    geom_line(aes(x = time, y = lower, group = cluster, color = cluster),
              data = band_long, size = rel(1), alpha = .5)
}
