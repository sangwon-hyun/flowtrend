# Generated from _main.Rmd: do not edit by hand

#' Pool the entire series of cytograms, fit a GMM model, and re-aggregate parameters.
#' 
#' @param ylist Data.
#' @param numclust Number of clusters.
#' @return
#' 
#' @export
underfit_gmm <- function(ylist, numclust){

  ## ## Tempoerary
  ## isignal = 0
  ## isim = 1
  ## destin = file.path("~/repos/flowtrend/inst/output/1dsim-even/data")
  ## datobj = readRDS(file = file.path(destin, paste0("isignal-", isignal,"-isim-", isim, "-", "datobj.RDS")) )
  ## datobj_new = readRDS(file = file.path(destin, paste0("isignal-", isignal,"-isim-", isim, "-", "datobj-new.RDS")) )
  ## ylist = datobj$ylist_unsorted
  ## numclust = 2
  ## ## end of temporary
  
  ## Pool all data
  TT <- length(ylist)
  nt <- sapply(ylist, nrow)
  ylist_pooled <- do.call(rbind, ylist) %>% as_tibble()
  colnames(ylist_pooled) = "y"

  ## Fit the GMM model
  gmm_pooled <- mclust::Mclust(data = ylist_pooled, G = numclust,
                                modelNames = "V", verbose = FALSE)

  ## Memberships (soft- and hard-clustered)
  hard_mem = gmm_pooled$z  %>% apply(1, which.max)
  soft_mem = gmm_pooled$z  %>%
    apply(1, function(myrow){
      sample(1:2, size = 1, replace = FALSE, prob=myrow)
    })

  ## Put together in a table
  tab <- tibble(y = ylist_pooled$y,
                soft_cluster = factor(soft_mem, levels = c(2,1)),
                hard_cluster = factor(hard_mem, levels = c(2,1)),
                time = rep(1:TT, times = nt)) 
  tab_long = tab %>%  pivot_longer(-c("time", "y"), values_to = "cluster",
                                   names_to = "type")
  ## ## Optional plotting code
  ## ggplot(tab_long) +
  ##   geom_point(aes(x=time, y=y, col = cluster), alpha = .5) +
  ##   facet_grid(cluster~type)

  ## ## We need this to return the same mu, prob, and sigma as before
  ## mu = obj_flowtrend$mn
  ## prob = obj_flowtrend$prob
  ## sigma = obj_flowtrend$sigma

  ## That's it! Return the results
  ## param_mat = matrix(NA, nrow = TT, ncol = 6)
  ## colnames(param_mat) = c("time", "mn1", "mn2", "prob1", "prob2", "sd1", "sd2")
  ## param_mat$time = 1:TT
  param_mat = tibble(time=1:TT,
                     mn1 = gmm_pooled$parameters$mean[1],
         mn2 = gmm_pooled$parameters$mean[2],
         sd1 = gmm_pooled$parameters$variance$sigmasq[1],
         sd2 = gmm_pooled$parameters$variance$sigmasq[2],
         prob1 = gmm_pooled$parameters$pro[1],
         prob2 = gmm_pooled$parameters$pro[2])
  mu = param_mat[,c("mn1", "mn2")] %>% as.matrix()
  prob = param_mat[,c("prob1", "prob2")] %>% as.matrix()
  sigma = param_mat[,c("sd1", "sd2")] %>% as.matrix()
  
  ## Soft membership
  memlist = tab_long %>% subset(type == "soft_cluster") %>% group_by(time) %>% 
    group_split() %>% lapply(function(a)pull(a, cluster))

  ## Responsibilities
  soft_mem = gmm_pooled$z %>% as_tibble()
  colnames(soft_mem) = c("clust1", "clust2")
  resp_list = soft_mem %>% add_column( time = rep(1:TT, times = nt))  %>% group_by(time) %>% group_split() %>%
    lapply(select, c("clust1", "clust2")) %>% lapply(as.matrix)

  ## List of sigmas
  TT = nrow(sigma)
  sigma_list = lapply(1:TT, function(tt){
    one_row = sigma[tt,] %>% as.numeric()
    one_sigma = array(NA, dim = c(2,1,1))
    one_sigma[,1,1] = one_row
    return(one_sigma)
  })

  return(list(tab_long = tab_long,
              param_mat = param_mat,
              mu = mu,
              prob = prob,
              sigma = sigma,
              sigma_list = sigma_list,
              memlist = memlist,
              resp_list = resp_list,
              numclust = numclust))
}
