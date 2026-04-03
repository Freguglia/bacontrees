test_that("Metropolis Hastings method for baConTree works", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 4, alpha = 0.001,
                      priorWeights = function(node) exp(-1/3*node$getDepth()))

  bt$runMetropolisHastings(100)
  bt$runMetropolisHastings(100)
  expect_s3_class(bt$getChain(), "data.frame")
})

test_that("baConTree initialization with priors works", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 4, alpha = 0.1, priorWeights = function(x) exp(-1/3*x$getDepth()))

  bt$runMetropolisHastings(100)
  bt$runMetropolisHastings(100)
  expect_s3_class(bt$getChain(), "data.frame")
})

test_that("chain grows across multiple runMetropolisHastings calls", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.001,
                      priorWeights = function(x) 1)
  bt$runMetropolisHastings(50)
  n_first <- nrow(bt$getChain())
  bt$runMetropolisHastings(50)
  expect_gt(nrow(bt$getChain()), n_first)
})
