# tests/testthat/test-treeFromContexts.R

test_that("treeFromContexts parses vector input correctly", {
  contexts <- c("*.0", "*.1.0", "*.1.1")
  tree <- treeFromContexts(contexts)

  expect_s3_class(tree, "ContextTree")
  expect_setequal(tree$getAlphabet()$symbols, c("0", "1"))

  expect_equal(tree$getMaximalDepth(), 2)

  active <- tree$getActiveNodes(idx = TRUE)
  expect_setequal(active, contexts)
})

test_that("treeFromContexts parses a single string with braces and commas", {
  input <- "{*.a, *.b, *.c.a, *.c.b, *.c.c}"
  tree <- treeFromContexts(input)

  expect_s3_class(tree, "ContextTree")

  expect_setequal(tree$getAlphabet()$symbols, c("a", "b", "c"))

  expect_equal(tree$getMaximalDepth(), 2)

  active <- tree$getActiveNodes(idx = TRUE)
  expect_setequal(active, c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c"))
})

test_that("treeFromContexts handles whitespace and varying spacing robustly", {
  input <- "{  *.x ,*.y,  *.z  }"
  tree <- treeFromContexts(input)

  expect_setequal(tree$getAlphabet()$symbols, c("x", "y", "z"))
  expect_equal(tree$getMaximalDepth(), 1)

  expect_setequal(tree$getActiveNodes(idx = TRUE), c("*.x", "*.y", "*.z"))
})

test_that("invalid trees fail", {
  contexts <- c("*.0", "*.1.0", "*.1.1", "*.2.0.1")
  expect_error(tree <- treeFromContexts(contexts), regexp = "full")
})

test_that("treeFromContexts returns identical output for equivalent inputs", {
  x1 <- c("*.a", "*.b.a", "*.b.b")
  x2 <- "{*.a, *.b.a, *.b.b}"

  t1 <- treeFromContexts(x1)
  t2 <- treeFromContexts(x2)

  expect_equal(t1$alphabet, t2$alphabet)
  expect_equal(t1$getMaximalDepth(), t2$getMaximalDepth())
  expect_setequal(t1$getActiveNodes(idx = TRUE), t2$getActiveNodes(idx = TRUE))
})
