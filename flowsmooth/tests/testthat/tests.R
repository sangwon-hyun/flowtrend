# Generated from _main.Rmd: do not edit by hand  
testthat::test_that("Trend filtering regression matrix is created correctly.", {
  H1 <- gen_tf_mat(10, 1)
  H2 <- gen_tf_mat(10, 1, x=(1:10)/10)
  testthat::expect_equal(H1, H2)
  testthat::expect_equal(dim(H1), c(10,10))
})

testthat::test_that("Test for softmax",{
  link = runif(100, min = -10, max = 10) %>% matrix(nrow = 10, ncol = 10)
  testthat::expect_true(all(abs(rowSums(softmax(link)) - 1) < 1E-13))
})

testthat::test_that("E step returns appropriately sized responsibilities.",{
  
  ## Generate some fake data
  TT = 100
  ylist = lapply(1:TT, function(tt){ runif(90) %>% matrix(ncol = 3, nrow = 30)})
  numclust = 3
  dimdat = 3

  ## Initialize a few parameters, not carefully
  sigma = init_sigma(ylist, numclust) ## (T x numclust x (dimdat x dimdat))
  mn = init_mn(ylist, numclust, TT, dimdat)##, countslist = countslist)
  prob = matrix(1/numclust, nrow = TT, ncol = numclust) ## Initialize to all 1/K.

  ## Calculate responsibility
  ## TODO: the code fails here. why?
  resp = Estep(mn = mn, sigma = sigma, prob = prob, ylist = ylist, numclust = numclust)

  ## Check these things
  testthat::expect_equal(length(resp), length(ylist))
  testthat::expect_equal(sapply(resp, dim), sapply(ylist, dim))
})

testthat::test_that("Mstep of pi returns a (T x K) matrix.", {

  ## Generate some fake responsibilities and trend filtering matrix
  TT = 100
  numclust = 3
  nt = 10
  resp = lapply(1:TT, function(tt){
    oneresp = runif(nt*numclust) %>% matrix(ncol=numclust)
    oneresp = oneresp/rowSums(oneresp)
  })
  H_tf <- gen_tf_mat(n = TT, k = 0)

  ## Check the size
  pred_link = Mstep_prob(resp, H_tf, l_prob = 0, lambda_prob = 1E-3)
  testthat::expect_equal(dim(pred_link), c(TT, numclust))
  pred_link = Mstep_prob(resp, H_tf)
  testthat::expect_equal(dim(pred_link), c(TT, numclust))

  ## Check the correctness
  pred_link = Mstep_prob(resp, H_tf)
})

testthat::test_that("Test the M step of \pi against CVXR", {})

testthat::test_that("Test the M step of \mu against CVXR", {})

testthat::test_that("The prediction function returns the right things", {
  ## Generate data
  set.seed(100)
  dt       <- gendat_1d(100, rep(100, 100), die_off_time = 0.45)
  ylist = dt %>% dt2ylist()
  x = dt %>% pull(time) %>% unique()
  obj <- flowsmooth(ylist = ylist,
                    x = x,
                    maxdev = 5,
                    numclust = 3,
                    lambda = 0.02,
                    l = 1,
                    l_prob = 2,
                    lambda_prob = .005, ## 
                    nrestart = 1,
                    niter = 3)

  predobj = predict_flowsmooth(obj)
  testthat::expect_named(predobj, c("mn", "prob", "sigma", "x"))
})

## Generate data
set.seed(100)
dt       <- gendat_1d(100, rep(100, 100), die_off_time = 0.45)
dt_model       <- gendat_1d(100, rep(100, 100), die_off_time = 0.45, return_model = TRUE)
held_out = 25:35
dt_subset = dt %>% subset(time %ni% held_out)
ylist = dt_subset %>% dt2ylist()
x = dt_subset %>% pull(time) %>% unique()
obj <- flowsmooth(ylist = ylist, 
                  x = x,
                  maxdev = 5,
                  numclust = 3,
                  lambda = 0.02,
                  l = 1,
                  l_prob = 2,
                  lambda_prob = .005, ## 
                  nrestart = 1)

## Also reorder the cluster labels of the truth, to match the fitted model.
ord = obj$mn[,1,] %>% colSums() %>% order(decreasing=TRUE)
lookup <- setNames(c(1:obj$numclust), ord)
dt_model$cluster = lookup[as.numeric(dt_model$cluster)] %>% as.factor()

## Reorder the cluster lables of the fitted model.
obj = reorder_clust(obj)

testthat::test_that("prediction function returns the right things", {
   
  predobj = predict_flowsmooth(obj, newtimes = held_out)

  ## Check a few things
  testthat::expect_equal(predobj$x,  held_out)
  testthat::expect_equal(rowSums(predobj$prob),  rep(1, length(held_out)))
  testthat::expect_equal(dim(predobj$mn),  c(length(held_out), 1, 3))
})

testthat::test_that("Objective value decreases over EM iterations.",{
  
  devtools::load_all("~/repos/FlowTF")
  for(iseed in 1:5){
    
    ## Generate increasingly noisy data
    set.seed(iseed*100)
    dt       <- gendat_1d(100, rep(100, 100), die_off_time = 0.2)
    dt$Y = dt$Y + rnorm(nrow(dt), 0, iseed/2)
    ylist = dt %>% dt2ylist()
    x = dt %>% pull(time) %>% unique()
    
    ## Fit model
    set.seed(0)
    obj <- flowsmooth(ylist = ylist,
                      x = x,
                      maxdev = 5,
                      numclust = 3,
                      lambda = 0.02,
                      l = 1,
                      l_prob = 2,
                      ## lambda_prob = .5,
                      lambda_prob = 0.05,
                      nrestart = 1)
    
    ## Test objective monotonicity
    testthat::expect_true(all(diff(obj$objective) < 0))
  }
})

