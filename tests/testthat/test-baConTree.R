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
