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

# Helper: number of valid context trees for an m-symbol alphabet and maximalDepth L.
# With constant prior weights = 1, sigmaPrior at the root equals this value.
# Recursion: start at 1 (leaf sigma), then apply s <- 1 + s^m L times.
n_trees <- function(m, L) {
  s <- 1
  for (i in seq_len(L)) s <- 1 + s^m
  s
}

test_that("getMarginalLikelihood returns correct types", {
  bt <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                      priorWeights = function(node) 1)

  log_ml <- bt$getMarginalLikelihood(log = TRUE)
  brob_ml <- bt$getMarginalLikelihood(log = FALSE)

  expect_type(log_ml, "double")
  expect_true(inherits(brob_ml, "brob"))
})

test_that("getMarginalLikelihood log=TRUE equals log of log=FALSE result", {
  bt <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                      priorWeights = function(node) 1)

  log_ml   <- bt$getMarginalLikelihood(log = TRUE)
  brob_ml  <- bt$getMarginalLikelihood(log = FALSE)

  expect_equal(log_ml, as.numeric(log(brob_ml)), tolerance = 1e-10)
})

test_that("sigmaPrior at root equals number of valid trees under constant prior", {
  # With priorWeights = 1 for all nodes, sigmaPrior at root = n_trees(m, L)
  # abc_vec uses a 3-symbol alphabet
  m <- 3

  for (L in 1:3) {
    bt <- baConTree$new(abc_vec, maximalDepth = L, alpha = 0.1,
                        priorWeights = function(node) 1)
    root_sigma_prior <- as.numeric(bt$nodes[["*"]]$extra$sigmaPrior)
    expect_equal(root_sigma_prior, n_trees(m, L),
                 label = paste0("sigmaPrior at root for maximalDepth=", L))
  }
})

test_that("getMarginalLikelihood is finite and negative for non-trivial data", {
  bt <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                      priorWeights = function(node) 1)

  log_ml <- bt$getMarginalLikelihood(log = TRUE)
  expect_true(is.finite(log_ml))
  expect_lt(log_ml, 0)
})
