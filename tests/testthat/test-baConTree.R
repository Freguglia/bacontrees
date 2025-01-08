test_that("Metropolis Hastings method for baConTree works", {
  set.seed(1)
  bt <- baConTree$new(abc_vec, maximalDepth = 4, active = "root")
  bt$setAllDirichletPars(0.001)
  bt$setContextPriorWeights(function(node) -1/3*node$getDepth())

  chain <- bt$runMetropolisHastings(100)
  expect_s3_class(chain, "data.frame")
})
