# bacontrees 0.0.4

## New features

* `treeFromContexts()` has been removed. Instead, `ContextTree` now exposes an
  `$activateFromContexts(contexts)` method that activates a specific tree on an
  existing `ContextTree` object. It accepts the same input format as the old
  function (a character vector of context paths, or a single brace-enclosed
  comma-separated string).

* `baConTree` nodes now store `priorBranchingProbability` and
  `posteriorBranchingProbability` in their `extra` list. These are computed
  during initialisation as `prod(children sigmaPrior) / sigmaPrior` (and the
  posterior analogue), representing the probability that the tree branches at
  that node rather than stopping.

* `baConTree` now exposes a `$sampleTree(type)` method for exact sampling from
  the prior (`type = "prior"`) or posterior (`type = "posterior"`) distribution
  over context trees. The method sets the active tree to the sampled tree and
  invisibly returns its tree code.

* `baConTree` now exposes a `$activateMap()` method that sets the active tree
  to the Maximum a Posteriori (MAP) tree — the context tree with the highest
  posterior probability. `baConTree$new()` also gains an `initialTree` argument
  (`"map"` or `"root"`, defaulting to `"map"`) to control which tree is active
  right after construction.

* `baConTree` now exposes a `$getMarginalLikelihood(log = TRUE)` method that
  returns the marginal likelihood of the data under the Bayesian context tree
  model (i.e. `sigmaPosterior / sigmaPrior` at the root, summed over all trees).
  Pass `log = TRUE` (default) for a plain R `numeric` on the log scale, or
  `log = FALSE` for the exact value as a `brob` object.

# bacontrees 0.0.3

## API changes

* The `priorWeights` argument in `baConTree$new()` and the `context_weights`
argument in `metropolis_vlmc()` now expect a function that returns weights on
the **natural scale** (i.e. non-log). The `log()` transformation is applied
internally. The default for `context_weights` has changed from `function(node) 0`
to `function(node) 1` to preserve the same uniform-prior behaviour.

* Each node's `extra` list now stores both `priorWeight` (natural scale, as
returned by the user-supplied function) and `logPriorWeight` (log scale, used
internally for posterior computations).

* In the `baConTree` class, `alpha` and `priorWeights` are now required arguments
in `$new()` and must be passed at initialization. The `$setAlpha()` and
`$setContextPriorWeights()` methods have been removed.

* The `active` argument has been removed from `ContextTree$new()` (and therefore
`baConTree$new()`). The tree is always initialized with the maximal active tree.
To start from the root, call `$activateRoot()` after construction.

* The `initial_root` argument has been removed from `metropolis_vlmc()`. The
chain always starts from the maximal tree.

## Internal / encapsulation improvements

* `ContextTree$nodes` is now a read-only active binding. External code can still
read nodes and mutate their attributes (e.g. `$extra`) via R6 reference semantics,
but structural changes to the node list (adding or removing nodes) are rejected
with an error.

* `TreeNode$counts` is now a private field exposed through a validated active
binding. The public interface is unchanged, but non-numeric assignments now throw
an error.

* `TreeNode$setChildrenPaths()` has been removed from the public API. Children
paths are now set internally by `ContextTree` via direct private-environment
access, as this operation is only meaningful during tree construction.

## Bug fixes

* Fixed `TreeNode` computing the wrong `parentPath` for alphabet symbols longer
than one character. The path was truncated by a fixed two-character offset
(`str_sub(..., end = -3)`) instead of splitting on `.` and dropping the last
segment.

* `ContextTree$activateByCode()` now correctly recomputes `growableNodes` and
`prunableNodes` after restoring a tree from a code, so subsequent `$growActive()`
and `$pruneActive()` calls behave correctly.

# bacontrees 0.0.2

## API changes

* `ContextTree` (and therefore) `baConTree` classes can now initialize with a `data`
argument instead of using `$setData()` method in a posterior call. The `Alphabet`
argument is not required if `data` is used, and the alphabet is inferred from
the data. `data` is the first argument, so initialization based on past implementation
with unnamed arguments will break.

* In the `ContextTree` class, the isLeaf attribute is now substituted by the private attribute isLeaf_. The public isLeaf() method is available to check if a node is a leaf.

* In the `baConTree` class, the `$setAllDirichletPars()` method was renamed to
`$setAlpha()`. Node attributes set by this class are now named: `marginalNodeLL`,
`priorWeight`, `posteriorWeight`, `childrenPosteriorWeight`, `growPosteriorRatio` and
`prunePosteriorRatio`.

## New features

* Added `$igraph()` method do `ContextTree` for converting it to an `igraph` object.
This object can be used for plotting with `ggraph` and contains the main attributes
and all `extra` attributes from nodes.

* Added `S3` `plot` methods for both `ContextTree` and `baConTree` classes.
