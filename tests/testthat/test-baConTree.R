test_that("Metropolis Hastings method for baConTree works", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 4, active = "root")
  bt$setAlpha(0.001)
  bt$setContextPriorWeights(function(node) -1/3*node$getDepth())

  bt$runMetropolisHastings(100)
  bt$runMetropolisHastings(100)
  expect_s3_class(bt$getChain(), "data.frame")
})

test_that("baConTree initialization with priors works", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 4, alpha = 0.1, priorWeights = function(x) -1/3*x$getDepth(), active = "root")

  bt$runMetropolisHastings(100)
  bt$runMetropolisHastings(100)
  expect_s3_class(bt$getChain(), "data.frame")
})

test_that("runMetropolisHastings errors if alpha is not set", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, active = "root")
  bt$setContextPriorWeights(function(node) 0)
  expect_error(bt$runMetropolisHastings(10))
})

test_that("runMetropolisHastings errors if context prior is not set", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, active = "root")
  bt$setAlpha(0.001)
  expect_error(bt$runMetropolisHastings(10))
})

test_that("setAlpha errors if called a second time", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, active = "root")
  bt$setAlpha(0.001)
  expect_error(bt$setAlpha(0.01), regexp = "already specified")
})

test_that("chain grows across multiple runMetropolisHastings calls", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.001,
                      priorWeights = function(x) 0, active = "root")
  bt$runMetropolisHastings(50)
  n_first <- nrow(bt$getChain())
  bt$runMetropolisHastings(50)
  expect_gt(nrow(bt$getChain()), n_first)
})
