# Generated from _main.Rmd: do not edit by hand

#' Makes a series of plots.
#' 
#' @param destin This directly has (1) summary.RDS, (2) meta.Rdata, and (3) datobj.RDS; optionally, it may have a dt_model
#' @return Nothing; but the pdf plots are created.
#' 
#' @export
plot_from_summary <- function(destin){
  ## Load the results 
  cvres = readRDS(file.path(destin, paste0("summary.RDS")))
  load(file.path(destin, paste0("meta.Rdata")), verbose=TRUE)
##  datobj = readRDS(file.path(destin, paste0("datobj.RDS")))
  if(file.exists(file.path(destin, "dt_model.RDS"))){
    dt.model = readRDS(file.path(destin, paste0("dt_model.RDS")))
  } else {
    dt.model = NULL
  }

  ##ylist = datobj$ybin_list %>% lapply(cbind)
  ##countslist = datobj$counts_list
  
  ## rows are prob, cols are means
  pdf(file.path(destin, "cvscoremat.pdf"), width = 6, height = 6)
  cvscoremat_lattice_plot =  
  cvres$cvscore.mat %>% drawmat_precise(ylab = "lambda_prob", xlab = "lambda_mean") +
    latticeExtra::layer(lattice::panel.points(x = cvres$min.inds[2],
                                              y = nrow(cvres$cvscore.mat) + 1 - cvres$min.inds[1], 
                                              pch = 4, col = "orange", cex=2)) +
    latticeExtra::layer(lattice::panel.points(x = cvres$min.inds.1se[2],
                                              y = nrow(cvres$cvscore.mat) + 1 - cvres$min.inds.1se[1], 
                                              pch = 4, col = "orange", cex=2))
  plot(cvscoremat_lattice_plot) ## Need to load library(lattice) before this.
  graphics.off()
  
  ## The minimum values.
  min_lam_prob = lambda_probs[cvres$min.inds[2]]
  min_lam_mean = lambda_means[cvres$min.inds[1]]
  
  ## each line is a different lambda_mean
  pdf(file.path(destin, "cvscoremat-lines-over-means.pdf"), width = 7, height = 7)
  a = cvres$cvscore.mat %>% t() %>% matplot(x = lambda_probs, type='o', col = 'grey', main = "each line is a different lambda_mean", log = "x")

  abline(v=min_lam_prob, col = 'red')
  graphics.off()
  
  pdf(file.path(destin, "cvscoremat-lines-over-probs.pdf"), width = 7, height = 7)
  cvres$cvscore.mat %>% matplot(x=lambda_means, type='o', col = 'grey', main = "each line is a different lambda_prob", log = "x")
  abline(v=min_lam_mean, col = 'red')
  graphics.off()

  n_lam_prob = nrow(cvres$cvscore.mat)
  n_lam_mean = ncol(cvres$cvscore.mat)
  
  ## Reorder clusters
  cvres$bestres = reorder_clust(cvres$bestres)
  for(ii in 1:n_lam_prob){
    print(ii)
    for(jj in 1:n_lam_mean){
      ## Reorder the cluster labels of the fitted ("best") models.
      reordered_obj = reorder_kl(newres = cvres$bestreslist[[paste0(ii, "-", jj)]],
                                 origres = cvres$bestres,##cvres$bestreslist[["1-1"]],
                                 ylist_particle = datobj$ylist)
      cvres$bestreslist[[paste0(ii, "-", jj)]] = reordered_obj
    }
  }
  
  ## Dependign on the dimension of the data, make a different plot
  ## if(obj$dimdat == 1) idim = 1
  ##  if(obj$dimdat > 1) idim = 1
  for(idim in 1:3){
    print(idim)

  ## Make plots of the "best" model
  g1 = flowtrend::plot_1d(ylist = ylist,
                          countslist = countslist,
                          obj=cvres$bestres,
                          idim = idim)
  if(!is.null(dt.model)){
    g1 = g1 + geom_line(aes(x = time, y = mean, group = cluster),
                        data = dt.model,## %>% subset(time %ni% held_out),
                        linetype = "dashed", size=1, alpha = .7)
  }
  
  g2 = flowtrend::plot_prob(cvres$bestres, x=1:length(ylist))
  if(!is.null(dt.model)){
    g2 = g2 + geom_line(aes(x=time, y=prob, group = cluster), linetype = "dashed",
                        data=dt.model  %>% select(time, cluster, prob) %>% unique())
  }
  do.call(ggpubr::ggarrange, c(list(g1, g2), ncol = 1, nrow = 2)) -> g
  g %>% ggsave(file = file.path(destin, paste0("mean-and-prob-idim-", idim, ".pdf")), width = 10, height=7)
  }
  
  
  ## Make all model plots
  lambda_means = colnames(cvres$cvscore.mat) %>% as.numeric()
  lambda_probs = rownames(cvres$cvscore.mat) %>% as.numeric()

  for(idim in 1:3){
    
  kk = 1
  glist = list()
  for(ii in 1:n_lam_prob){
    for(jj in 1:n_lam_mean){

      ## Calculate number of knots (complexity, df)
      knots_mean = sapply(1:2, function(iclust){
        abs_means = cvres$bestreslist[[paste0(ii, "-", jj)]] %>% .$mn %>% .[,1,iclust] %>% diff(differences=3) %>% abs()
        df = sum(abs_means > 1E-4)
      })
      knots_prob = sapply(1:2, function(iclust){
        abs_probs = cvres$bestreslist[[paste0(ii, "-", jj)]] %>% .$prob_link %>% .[,iclust] %>% diff(differences=2) %>% abs()
        df = sum(abs_probs > 1E-4)
      })

      ## Visualize
      g = flowtrend::plot_1d(ylist = ylist, countslist = countslist, obj = cvres$bestreslist[[paste0(ii, "-", jj)]], idim = idim)
      g = g + ggtitle(paste0("lam=", signif(lambda_means[[jj]],3), ", lam_prob=", signif(lambda_probs[[ii]], 3)))
      g = g + theme(legend.position="none")

      g = g + annotate("text",  x=Inf, y = Inf,
                       label = paste0("knots(prob,mean)=", knots_prob,", ",knots_mean), vjust=1, hjust=1)


      optimal = ((ii == cvres$min.inds[1]) & (jj == cvres$min.inds[2]))
      onese = ((ii == cvres$min.inds.1se[1]) & (jj == cvres$min.inds.1se[2]))
      if(optimal | onese){
        g = g + theme(panel.border = element_rect(colour = "black", fill=NA, size=5))
      }

      ## Save plot
      glist[[kk]] = g
      kk = kk + 1
    }
  }
  all_bestres = do.call(ggpubr::ggarrange, c(glist, ncol = n_lam_mean, nrow = n_lam_prob))
  all_bestres %>%
    ggsave(file = file.path(destin, paste0("all_bestres-idim-", idim, ".pdf")),
           width = 40, height=30)
  }
  
  ##  All probabilities
  lambda_means = colnames(cvres$cvscore.mat) %>% as.numeric()
  kk = 1
  glist = list()
  for(ii in 1:n_lam_prob){
    for(jj in 1:n_lam_mean){
      ## Visualize
      g = flowtrend::plot_prob(obj = cvres$bestreslist[[paste0(ii, "-", jj)]], x=1:length(ylist))
      g = g + ggtitle(paste0("lam=", signif(lambda_means[[jj]],3), ", lam_prob=", signif(lambda_probs[[ii]], 3)))
      if(!is.null(dt.model)){
        g = g + geom_line(aes(x = time, y = prob, group = cluster),##, color = cluster),
                          data = dt.model, linetype = "dashed") 
      }
      g = g + theme(legend.position = "none")

      optimal = ((ii == cvres$min.inds[1]) & (jj == cvres$min.inds[2]))
      onese = ((ii == cvres$min.inds.1se[1]) & (jj == cvres$min.inds.1se[2]))
      if(optimal | onese){
        g = g + theme(panel.border = element_rect(colour = "black", fill=NA, size=5))
      }
  
      ## Save plot
      glist[[kk]] = g
      kk = kk + 1
    }
  }
  
  all_probs = do.call(ggpubr::ggarrange, c(glist, ncol=n_lam_mean, nrow=n_lam_prob))
  all_probs %>%
    ggsave(file = file.path(destin, paste0("all_probs.pdf")),
           width = 40, height=30)
  list_of_plots = list(all_probs = all_probs, all_bestres = all_bestres,
                       cvscoremat_lattice_plot = cvscoremat_lattice_plot ,
                       mean_plot = g1, prob_plot = g2)
  return(list_of_plots)
}
