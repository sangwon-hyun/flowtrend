# Generated from _main.Rmd: do not edit by hand

#' Simple plotter for 2d particle data.
plot_2d <- function(ylist, tt){

  ## Get data from one timepoint
  y = ylist %>% .[[tt]]
  y = y %>% as_tibble()
  colnames(y) = paste0("dim", c(1,2))

  ## Make a simple scatterplot
  y %>% ggplot() +
    geom_point(aes(x=dim1, y=dim2), alpha = .3) +
    theme_minimal() +
    coord_fixed()
}
