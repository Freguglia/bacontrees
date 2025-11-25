# bacontrees 0.0.1

## API changes

* `ContextTree` (and therefore) `baConTree` classes can now initialize with a `data`
argument instead of using `$setData()` method in a posterior call. The `Alphabet`
argument is not required if `data` is used, and the alphabet is inferred from
the data. `data` is the first argument, so initialization based on past implementation
with unnamed arguments will break.

* In the ContextTree class, the isLeaf attribute is now substituted by the private attribute isLeaf_. The public isLeaf() method is available to check if a node is a leaf.

## New features

* Added `$igraph()` method do `ContextTree` for converting it to an `igraph` object.
This object can be used for plotting with `ggraph`.
