# Generated from _main.Rmd: do not edit by hand

#' Simple plotter for 2d particle data.
plot_2d <- function(ylist, obj = NULL, tt){

  ## Basic checks
  stopifnot(ncol(ylist[[1]]) == 2)
  if(!is.null(obj)) stopifnot(obj$dimdat == 2)

  ## Get data from one timepoint
  y = ylist %>% .[[tt]]
  y = y %>% as_tibble()
  colnames(y) = paste0("dim", c(1,2))

  ## Make a simple scatterplot
  p = y %>% ggplot() +
    geom_point(aes(x=dim1, y=dim2), alpha = .2) +
    theme_minimal() +
    coord_fixed()


  p = p + ggtitle(paste0("Time=", tt)) +
    coord_cartesian(xlim = c(-6.5, 2.5), ylim = c(-6.5, 2.5)) +
    theme_minimal()

  ## Adding visualizations of the model |obj|
  if(is.null(obj)){
    return(p)
  } else {
    mnlist = lapply(1:obj$numclust, function(iclust){
      one_mnmat = obj$mn[,,iclust]
      colnames(one_mnmat) = paste0("dim", 1:2)
      one_mnmat %>% as_tibble() %>% add_column(cluster = iclust)
    })
    mnmat = do.call(rbind, mnlist)

    mn_colours = rep("red", 3)
    for(iclust in 1:obj$numclust){

      ## Add ellipse
      el = ellipse::ellipse(x = obj$sigma[iclust,,],
                            centre = obj$mn[tt,,iclust]) %>% as_tibble()
      p = p + geom_path(aes(x = x, y = y), data = el,
                        colour = mn_colours[iclust], lty = 2,
                        lwd = pmin(obj$prob[tt,iclust] * 8, 0.8))

      ## Add mean
      p = p + geom_point(aes(x = dim1, y = dim2),
                         data = mnmat %>% subset(cluster == iclust) %>% .[tt,],
                         colour = mn_colours[iclust],
                         ## size = rel(3))
                         size = obj$prob[tt,iclust] * 10)
    }
  }
  return(p)
}
