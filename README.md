
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bacontrees

<!-- badges: start -->

[![R-CMD-check](https://github.com/Freguglia/bacontrees/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Freguglia/bacontrees/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Freguglia/bacontrees/graph/badge.svg)](https://app.codecov.io/gh/Freguglia/bacontrees)
<!-- badges: end -->

`bacontrees` is an R package for modelling discrete sequential data
using **Ba**yesian **Con**text **Trees** (BaCon Trees). Context trees,
also known as Variable Length Markov Chains (VLMCs), are parsimonious
Markov models where the order of dependence can vary with the observed
past. `bacontrees` provides:

- A **generic tree structure** (`ContextTree`) that exposes the full
  context tree for building custom algorithms.
- **Exact Bayesian inference** (`baConTree`): closed-form marginal
  likelihood, exact MAP tree, exact prior/posterior probabilities for
  any tree, and exact sampling from the posterior — all via an efficient
  recursive algorithm.
- A **frequentist** estimator: `fit_vlmc()` estimates the context tree
  via the context algorithm with likelihood-ratio pruning.
- Simulation utilities (`rvlmc()`).

## Installation

You can install the development version of bacontrees from
[GitHub](https://github.com/Freguglia/bacontrees) with:

``` r
# install.packages("pak")
pak::pak("Freguglia/bacontrees")
```

Or with **devtools**:

``` r
# install.packages("devtools")
devtools::install_github("Freguglia/bacontrees")
```

## Background

A **Variable Length Markov Chain** (VLMC) is a Markov model where the
relevant past (the *context*) that determines the next symbol’s
distribution has a variable length. Further pasts are only distinguished
when the data supports it, making VLMCs much more parsimonious than
fixed-order Markov chains.

The set of contexts forms a complete subtree of the full (maximal)
suffix tree over the alphabet. `bacontrees` represents this subtree
explicitly as a `ContextTree` object, whose *active* leaf nodes are
exactly the contexts of the model.

Under the Bayesian formulation, a Dirichlet prior is placed on the
transition probabilities at each context and a prior distribution over
tree topologies is defined through per-node weights. The resulting
posterior over trees can be computed **exactly**: a single bottom-up
recursion over the maximal tree yields the marginal likelihood, the
posterior probability of every candidate tree, the MAP tree, and an
exact sampler — all without MCMC.

## Usage

### Included data

The package ships with two example datasets: `abc_vec` (a single
sequence of length 1000) and `abc_list` (a list of three sequences of
length 1000 each), generated from a three-symbol VLMC with alphabet
`{a, b, c}`.

``` r
library(bacontrees)
data(abc_list)
head(abc_list[[1]], 20)
#>  [1] "a" "c" "a" "c" "a" "c" "c" "b" "c" "b" "a" "c" "c" "b" "c" "b" "c" "b" "b"
#> [20] "c"
```

### Simulating sequences from a VLMC

Use `rvlmc()` to generate synthetic sequences given a context list and
the corresponding transition probability vectors.

``` r
alphabet      <- c("a", "b", "c")
context_list  <- c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c")
context_probs <- list(
  c(0.10, 0.20, 0.70),  # after 'a'
  c(0.33, 0.33, 0.34),  # after 'b'
  c(0.20, 0.10, 0.70),  # after 'ac'
  c(0.01, 0.98, 0.01),  # after 'bc'
  c(0.40, 0.40, 0.20)   # after 'cc'
)

set.seed(42)
seq1 <- rvlmc(1000, alphabet, context_list, context_probs)
head(seq1, 20)
#>  [1] "a" "a" "a" "a" "c" "c" "b" "a" "b" "a" "a" "c" "c" "a" "c" "c" "b" "c" "b"
#> [20] "b"
```

### Frequentist fitting — `fit_vlmc()`

`fit_vlmc()` implements the classical context algorithm: build a maximal
tree, then prune nodes whose likelihood-ratio test statistic falls below
`cutoff`.

``` r
fit <- fit_vlmc(abc_list, cutoff = 20, max_length = 4)
fit$getActiveNodes()
#> [1] "*.a"   "*.b"   "*.c.a" "*.c.b" "*.c.c"
```

### Visualising a context tree

`ContextTree` (and `baConTree`) objects have an S3 `plot()` method that
uses **ggraph** internally and can be customised like any ggplot2
object.

``` r
plot(fit)
```

<img src="man/figures/README-plot-1.png" alt="Context tree plot" width="100%" />

### Exact Bayesian inference — `baConTree`

`baConTree` extends `ContextTree` with Bayesian machinery. On
construction it runs a bottom-up recursion that precomputes the
normalising constants ($\sigma$-values) at every node of the maximal
tree. These values make all key Bayesian quantities available as direct,
closed-form computations.

``` r
bt <- baConTree$new(abc_list, maximalDepth = 3, alpha = 0.01,
                    priorWeights = function(node) exp(-node$getDepth() / 3))
```

#### Marginal likelihood

The exact log marginal likelihood of the data (integrating over all tree
topologies and all transition probability matrices) is available
immediately after construction:

``` r
bt$getMarginalLikelihood(log = TRUE)
#> [1] -2665.658
```

#### MAP tree

The MAP tree is found exactly in a single top-down pass — no sampling
required:

``` r
bt$activateMap()
bt$getActiveNodes()
#> [1] "*.a"   "*.c.a" "*.b"   "*.c.b" "*.c.c"
```

#### Exact posterior probabilities

The prior and posterior probability of *any* candidate tree can be
evaluated exactly by activating that tree and calling
`activeTreeProbabilities()`:

``` r
bt$activateFromContexts(c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c"))
bt$activeTreeProbabilities()
#> $prior
#> [1] 0.04045974
#> 
#> $posterior
#> [1] 0.9998251
```

#### Exact sampling

Draw independent samples directly from the prior or posterior without
MCMC:

``` r
set.seed(1)
bt$sampleTree("posterior")
#> [1] "AAAAADNAAA"
bt$getActiveNodes()
#> [1] "*.a"   "*.c.a" "*.b"   "*.c.b" "*.c.c"
```

### Metropolis-Hastings sampling — `metropolis_vlmc()`

For settings where exact enumeration is not the goal (e.g., diagnosing
multimodality or studying the mixing behaviour of MCMC on tree spaces),
`metropolis_vlmc()` provides a Metropolis-Hastings sampler over context
trees.

``` r
library(progressr)

with_progress({
  result <- metropolis_vlmc(
    abc_list,
    n_steps       = 2000,
    max_depth     = 3,
    alpha         = 0.01,
    context_weights = function(node) exp(-node$getDepth() / 3),
    burnin        = 200
  )
})

print(result)
#> Tree with highest posterior probability ( 1 ):
#> { *.a, *.c.a, *.b, *.c.b, *.c.c }
#> # A tibble: 1 × 4
#>   tree_contexts                prob     n tree_code 
#>   <chr>                       <dbl> <int> <chr>     
#> 1 {*.a,*.c.a,*.b,*.c.b,*.c.c}     1  1800 AAAAADNAAA
```

The returned object contains:

| Field        | Description                                                 |
|--------------|-------------------------------------------------------------|
| `$df`        | Data frame of context sets ranked by posterior probability  |
| `$codes`     | Named list mapping tree codes to their context vectors      |
| `$chain`     | Full MCMC chain of tree codes (for convergence diagnostics) |
| `$baConTree` | The `baConTree` object used internally                      |

## Class reference

### `ContextTree`

The core data structure. Manages the maximal suffix tree, tracks which
nodes are *active* (the current context set), and handles data
attachment. Designed to be subclassed or used directly for building
custom algorithms over context trees.

| Method | Description |
|----|----|
| `$new(dataset, maximalDepth, alphabet)` | Construct a context tree |
| `$root()` | Return the root node |
| `$validate()` | Check that the maximal tree structure is valid |
| `$getMaximalDepth()` | Return the maximal depth of the tree |
| `$getActiveNodes()` | Return paths of active (leaf) nodes |
| `$getLeaves()` | Return paths of maximal-tree leaves |
| `$getInnerNodes()` | Return inner nodes of the active tree |
| `$getGrowableNodes()` | Return active nodes that can be grown |
| `$getPrunableNodes()` | Return active nodes that can be pruned |
| `$nodeExists(path)` | Check if a node with the given path exists |
| `$getParentNode(path)` | Return the parent of a node |
| `$getChildrenNodes(path)` | Return the children of a node |
| `$getSiblingNodes(path)` | Return the siblings of a node |
| `$activateRoot()` | Set active tree to root-only |
| `$activateMaximal()` | Set active tree to all maximal leaves |
| `$activateByCode(code)` | Restore a previously stored tree code |
| `$activateFromContexts(contexts)` | Set active tree to match a context vector or string |
| `$growActive(path)` | Grow the active tree at a given node |
| `$pruneActive(path)` | Prune the active tree at a given node |
| `$setData(dataset)` | Attach sequence data and compute counts |
| `$getDataset()` | Retrieve attached `Sequence` object |
| `$getAlphabet()` | Retrieve the `Alphabet` object |
| `$activeTreeCode()` | Compact binary encoding of the active tree |
| `$igraph()` | Convert to an `igraph` object |
| `plot(ct)` | Plot with **ggraph** |

### `baConTree`

Extends `ContextTree` with exact Bayesian inference. The constructor
runs a bottom-up recursion to precompute the normalising constants for
the prior and posterior, enabling all quantities below to be evaluated
without MCMC.

| Method | Description |
|----|----|
| `$new(data, maximalDepth, alpha, priorWeights, initialTree)` | Construct a Bayesian context tree and precompute recursive quantities |
| `$getMarginalLikelihood(log)` | Exact marginal likelihood of the data |
| `$activateMap()` | Set active tree to the exact MAP tree |
| `$activeTreeProbabilities(log)` | Exact prior and posterior probabilities of the active tree |
| `$sampleTree(type)` | Draw an exact independent sample from the prior or posterior |
| `$runMetropolisHastings(steps)` | Run a Metropolis-Hastings MCMC sampler (optional) |
| `$getChain()` | Retrieve the MCMC chain as a data frame |
