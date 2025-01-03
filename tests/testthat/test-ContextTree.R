test_that("Initialization works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(alphabet = alphabet, maximalDepth = 3)

  expect_true(tree$nodeExists("*"))
  expect_equal(tree$root()$getPath(), "*")
})

test_that("Adding and retrieving nodes works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(alphabet = alphabet, maximalDepth = 1)

  expect_true(tree$nodeExists("*.A"))
  expect_true(tree$nodeExists("*.B"))
  expect_true(tree$nodeExists("*.C"))

  node <- tree$nodes[["*.A"]]
  expect_equal(node$getPath(), "*.A")
})

test_that("Validation function works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(alphabet = alphabet, maximalDepth = 1)

  expect_true(tree$validate())

  # Manually add an invalid node
  tree$nodes[["*.D"]] <- TreeNode$new("*.D")
  expect_false(tree$validate())
})

test_that("Active nodes are correctly identified", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(alphabet = alphabet, maximalDepth = 2)
  tree$activateRoot()

  expect_equal(tree$getActiveNodes(), "*")

  tree$growActive("*")
  expect_equal(tree$getActiveNodes(), c("*.A", "*.B", "*.C"))

  tree$growActive("*.A")
  expect_equal(sort(tree$getActiveNodes()),
               sort(c("*.A.A","*.A.B", "*.A.C", "*.B", "*.C")))
})

test_that("Fill by depth works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(alphabet = alphabet, maximalDepth = 2)

  leaves <- tree$getLeaves()
  expected_leaves <- c(
    "*.A.A", "*.A.B", "*.A.C",
    "*.B.A", "*.B.B", "*.B.C",
    "*.C.A", "*.C.B", "*.C.C"
  )
  expect_equal(sort(leaves), sort(expected_leaves))
})

test_that("Growable Nodes are correctly identified", {
  tree <- ContextTree$new(alphabet = LETTERS[1:3], maximalDepth = 2, active = "root")

  expect_setequal(tree$getGrowableNodes(), "*")
  tree$growActive("*")
  tree$growActive("*.A")
  expect_setequal(tree$getGrowableNodes(), c("*.B", "*.C"))

  tree$pruneActive("*.A.A")
  expect_setequal(tree$getGrowableNodes(), c("*.A", "*.B", "*.C"))

  tree$growActive("*.A")
  tree$growActive("*.B")
  tree$growActive("*.C")
  expect_length(tree$getGrowableNodes(), 0)
})

test_that("Prunable Nodes are correctly identified", {
  tree <- ContextTree$new(alphabet = LETTERS[1:3], maximalDepth = 2, active = "root")

  expect_length(tree$getPrunableNodes(), 0)
  tree$growActive("*")

  expect_setequal(tree$getPrunableNodes(), c("*.A", "*.B", "*.C"))
  tree$growActive("*.A")

  expect_setequal(tree$getPrunableNodes(), c("*.A.A", "*.A.B", "*.A.C"))
  expect_error(tree$pruneActive("*.B"))
  tree$pruneActive("*.A.B")

  expect_setequal(tree$getPrunableNodes(), c("*.A", "*.B", "*.C"))
  tree$pruneActive("*.A")

  expect_error(tree$pruneActive("*"))
  tree$growActive("*")
  expect_setequal(tree$getPrunableNodes(), c("*.A", "*.B", "*.C"))
})
