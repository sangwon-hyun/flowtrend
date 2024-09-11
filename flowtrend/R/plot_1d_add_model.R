# Generated from _main.Rmd: do not edit by hand

#' Add a model.
#' 
#' @param obj Model
#' @param gg0 result of plot_1d
#' @param idim dimension
#' @param plot_band If TRUE, plot a +1.96 standard deviation band.
#'
#' @return ggplot object
#'
#' @export
plot_1d_add_model <- function(gg0, obj, idim, plot_band = TRUE){
  times = obj$x ## 1:(obj$TT) ## resolve this!
  numclust = obj$numclust
  mnmat = obj$mn %>% .[,idim,] %>%  `colnames<-`(1:numclust) %>% as_tibble() %>% 
    add_column(time = times)
  probmat = obj$prob %>% as_tibble() %>% setNames(1:numclust) %>% add_column(time = times)
  mn_long = mnmat %>% pivot_longer(-time, names_to = "cluster", values_to = "mean") %>%
    mutate(cluster = factor(cluster, levels=sapply(1:10, toString)))
  prob_long = probmat %>% pivot_longer(-time, names_to = "cluster", values_to = "prob") %>% 
    mutate(cluster = factor(cluster, levels=sapply(1:10, toString)))
  est_long = full_join(mn_long, prob_long, by = c("time","cluster"))
  ## est_long = est_long %>% mutate(cluster = factor(cluster, levels=sapply(1:10, toString)))
  gg = gg0 + geom_path(aes(x = time, y = mean, linewidth = prob, group = cluster, color = cluster),
                       data = est_long, lineend = "round", linejoin="mitre") +
    scale_linewidth(range = c(0,1))

  ## Add the estimated 95% probability regions for data.
  if(plot_band){
    stdev = obj$sigma %>% .[,idim,idim] %>% sqrt()
    names(stdev) = sapply(1:numclust, toString)
    mn_long_by_clust = mn_long %>% group_by(cluster) %>% group_split()
    band_long_by_clust = lapply(1:numclust, function(iclust){
      mn_long_by_clust[[iclust]] %>%
        mutate(upper = mean + 1.96 * stdev[iclust]) %>%
        mutate(lower = mean - 1.96 * stdev[iclust])
    })
    band_long = band_long_by_clust %>% bind_rows()
    gg = gg + geom_line(aes(x = time, y = upper, group = cluster, color = cluster),
                        data = band_long, size = rel(.7), alpha = .5) +
      geom_line(aes(x = time, y = lower, group = cluster, color = cluster),
                data = band_long, size = rel(.7), alpha = .5) +
      guides(size = "none") # To turn off line size from legend
  }
  return(gg)
}
