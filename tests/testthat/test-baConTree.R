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

# --- Branching probability tests ---

test_that("priorBranchingProbability is 0 for maximal-depth leaf nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                      priorWeights = function(node) 1)
  L <- bt$getMaximalDepth()
  leaf_nodes <- bt$nodes[sapply(bt$nodes, function(n) n$getDepth() == L)]
  probs <- sapply(leaf_nodes, function(n) n$extra$priorBranchingProbability)
  expect_true(all(probs == 0))
})

test_that("posteriorBranchingProbability is 0 for maximal-depth leaf nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                      priorWeights = function(node) 1)
  L <- bt$getMaximalDepth()
  leaf_nodes <- bt$nodes[sapply(bt$nodes, function(n) n$getDepth() == L)]
  probs <- sapply(leaf_nodes, function(n) n$extra$posteriorBranchingProbability)
  expect_true(all(probs == 0))
})

test_that("priorBranchingProbability is in [0, 1] for all nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  probs <- sapply(bt$nodes, function(n) n$extra$priorBranchingProbability)
  expect_true(all(probs >= 0 & probs <= 1))
})

test_that("posteriorBranchingProbability is in [0, 1] for all nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  probs <- sapply(bt$nodes, function(n) n$extra$posteriorBranchingProbability)
  expect_true(all(probs >= 0 & probs <= 1))
})

test_that("priorBranchingProbability at root equals correct value under constant prior", {
  # With priorWeights = 1, m = 3, L = 1: sigmaPrior(root) = 1 + 1^3 = 2
  # branchingProb(root) = 1/2
  bt <- baConTree$new(abc_vec, maximalDepth = 1, alpha = 0.1,
                      priorWeights = function(node) 1)
  expect_equal(bt$nodes[["*"]]$extra$priorBranchingProbability, 0.5)
})

test_that("priorBranching + stopping probability = 1 for all non-maximal nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  L <- bt$getMaximalDepth()
  non_leaf_nodes <- bt$nodes[sapply(bt$nodes, function(n) n$getDepth() < L)]
  for (node in non_leaf_nodes) {
    stopping_prob <- as.numeric(node$extra$priorWeight / node$extra$sigmaPrior)
    branching_prob <- node$extra$priorBranchingProbability
    expect_equal(stopping_prob + branching_prob, 1, tolerance = 1e-10)
  }
})

test_that("posteriorBranching + stopping probability = 1 for all non-maximal nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  L <- bt$getMaximalDepth()
  non_leaf_nodes <- bt$nodes[sapply(bt$nodes, function(n) n$getDepth() < L)]
  for (node in non_leaf_nodes) {
    stopping_prob <- as.numeric(node$extra$posteriorWeight / node$extra$sigmaPosterior)
    branching_prob <- node$extra$posteriorBranchingProbability
    expect_equal(stopping_prob + branching_prob, 1, tolerance = 1e-10)
  }
})

# --- sampleTree tests ---

test_that("sampleTree returns a character tree code", {
  set.seed(42)
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  code <- bt$sampleTree("prior")
  expect_type(code, "character")
})

test_that("sampleTree is reproducible with set.seed", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) 1)
  set.seed(7)
  code1 <- bt$sampleTree("prior")
  set.seed(7)
  code2 <- bt$sampleTree("prior")
  expect_equal(code1, code2)
})

test_that("sampleTree returns root-only tree when root branchingProbability is effectively 0", {
  # With priorWeight(root) >> prod(children_sigmaPrior), branchingProb -> 0
  # L=1, m=3: prod(children_sigmaPrior) = 1^3 = 1; root priorWeight = 1e300
  # branchingProb(root) = 1 / (1e300 + 1) ~ 1e-300 (converts to 0 as numeric)
  bt <- baConTree$new(abc_vec, maximalDepth = 1, alpha = 0.1,
                      priorWeights = function(node) if (node$getDepth() == 0) 1e300 else 1)
  bt$activateRoot()
  root_only_code <- bt$activeTreeCode()
  for (i in 1:10) {
    bt$sampleTree("prior")
    expect_equal(bt$activeTreeCode(), root_only_code)
  }
})

# --- activateMap tests ---

test_that("activateMap returns the baConTree object invisibly", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  result <- bt$activateMap()
  expect_identical(result, bt)
})

test_that("activateMap sets only isMapLeaf nodes active", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  bt$activateMap()
  for (node in bt$getActiveNodes(FALSE)) {
    expect_true(node$extra$isMapLeaf)
  }

  expect_setequal(bt$getActiveNodes(TRUE),
                  c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c"))
})

test_that("activateMap posterior >= root-only tree posterior", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  bt$activateMap()
  map_log_post <- bt$activeTreeProbabilities(log = TRUE)$log_posterior

  bt$activateRoot()
  root_log_post <- bt$activeTreeProbabilities(log = TRUE)$log_posterior
  expect_gte(map_log_post, root_log_post)
})

test_that("activateMap is idempotent", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  bt$activateMap()
  code1 <- bt$activeTreeCode()
  bt$activateMap()
  code2 <- bt$activeTreeCode()
  expect_equal(code1, code2)
})

# --- initialTree argument tests ---

test_that("initialTree = 'map' matches activateMap result", {
  bt_root <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                           priorWeights = function(node) exp(-1/3 * node$getDepth()),
                           initialTree = "root")
  bt_root$activateMap()

  bt_map <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                          priorWeights = function(node) exp(-1/3 * node$getDepth()),
                          initialTree = "map")

  expect_equal(bt_root$activeTreeCode(), bt_map$activeTreeCode())
})

test_that("initialTree = 'map' is the default and starts at map tree", {
  bt_default <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                              priorWeights = function(node) 1)
  bt_explicit <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                               priorWeights = function(node) 1, initialTree = "map")
  expect_equal(bt_default$activeTreeCode(), bt_explicit$activeTreeCode())
})

test_that("isMapLeaf is TRUE for all maximal-depth nodes", {
  bt <- baConTree$new(abc_vec, maximalDepth = 2, alpha = 0.1,
                      priorWeights = function(node) 1)
  L <- bt$getMaximalDepth()
  leaf_nodes <- bt$nodes[sapply(bt$nodes, function(n) n$getDepth() == L)]
  for (node in leaf_nodes) {
    expect_true(node$extra$isMapLeaf)
  }
})

test_that("bestPosterior at each node >= its own posteriorWeight", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()))
  for (node in bt$nodes) {
    expect_gte(as.numeric(node$extra$bestPosterior), as.numeric(node$extra$posteriorWeight) - 1e-10)
  }
})

test_that("initialTree = 'root' starts at the root-only tree", {
  bt <- baConTree$new(abc_vec, maximalDepth = 3, alpha = 0.1,
                      priorWeights = function(node) exp(-1/3 * node$getDepth()),
                      initialTree = "root")
  expect_length(bt$getActiveNodes(FALSE), 1)
  expect_equal(bt$getActiveNodes(TRUE), "*")
})
