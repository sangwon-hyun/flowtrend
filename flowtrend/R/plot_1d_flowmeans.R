# Generated from _main.Rmd: do not edit by hand

#' Plotter for underfit and overfit flowmeans.
#' @param ylist Data
#' @param obj Output from \code{underfit_flowmeans()}
#' @return ggplot object
#' @export
plot_1d_flowmeans <- function(obj, ylist){
  ## Make plot of only data

  gg = plot_1d(ylist)

  my_wrangle <- function(a, values_to){
    a %>% as_tibble() %>% setNames(c("1", "2", "3")) %>%
      mutate(time=row_number()) %>%
      pivot_longer(-time, names_to = "cluster", values_to = values_to)
  }

  numclust = obj$numclust
  mn_long = obj$mu %>% my_wrangle("mean")
  prob_long = obj$prob %>% my_wrangle("prob")
  ## prob_long = probmat %>% pivot_longer(-time, names_to = "cluster", values_to = "prob")
  est_long = full_join(mn_long, prob_long, by = c("time","cluster"))
  gg = gg + geom_path(aes(x = time, y = mean, linewidth = prob, group = cluster, color = cluster),
                        data = est_long,
                        lineend = "round", linejoin="mitre")
  
  ## Add the estimated 95% probability regions for data.
  ## stdev = obj$sigma %>% .[,,1] %>% sqrt()
  sigma_long = obj$sigma %>% sqrt() %>% my_wrangle("stdev")
  band_long = full_join(mn_long, sigma_long, by = c("time", "cluster")) %>%
  mutate(upper = mean + 1.96 * stdev)  %>% 
  mutate(lower = mean - 1.96 * stdev)

  gg = gg + geom_line(aes(x = time, y = upper, group = cluster, color = cluster),
                 data = band_long, size = rel(.7), alpha = .5) +
    geom_line(aes(x = time, y = lower, group = cluster, color = cluster),
              data = band_long, size = rel(.7), alpha = .5) +
    guides(size = "none") # To turn off line size from legend
  return(gg)
}
