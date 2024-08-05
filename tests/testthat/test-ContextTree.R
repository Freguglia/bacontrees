test_that("Initialization works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(Alphabet = alphabet)

  expect_equal(tree$Alphabet, alphabet)
  expect_equal(tree$m, length(alphabet$symbols))
  expect_true(tree$nodeExists("*"))
  expect_equal(tree$root$getPath(), "*")
})

test_that("Adding and retrieving nodes works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(Alphabet = alphabet)

  tree$addChildren("*")
  expect_true(tree$nodeExists("*.A"))
  expect_true(tree$nodeExists("*.B"))
  expect_true(tree$nodeExists("*.C"))

  node <- tree$getNode("*.A")
  expect_equal(node$getPath(), "*.A")
})

test_that("Validation function works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(Alphabet = alphabet)

  tree$addChildren("*")
  expect_true(tree$validate())

  # Manually add an invalid node
  tree$nodes[["*.D"]] <- TreeNode$new("*.D")
  expect_false(tree$validate())
})

test_that("Leaf nodes are correctly identified", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(Alphabet = alphabet)

  expect_equal(tree$getLeaves(), "*")

  tree$addChildren("*")
  expect_equal(tree$getLeaves(), c("*.A", "*.B", "*.C"))

  tree$addChildren("*.A")
  expect_equal(sort(tree$getLeaves()),
               sort(c("*.A.A","*.A.B", "*.A.C", "*.B", "*.C")))
})

test_that("Errors are handled correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(Alphabet = alphabet)

  expect_error(tree$addChildren("*.D"), "Cannot add children to \\*\\.D because it is not a node.")
})

test_that("Fill by depth works correctly", {
  alphabet <- Alphabet$new(LETTERS[1:3])
  tree <- ContextTree$new(Alphabet = alphabet)

  tree$fillByDepth(2)
  leaves <- tree$getLeaves()
  expected_leaves <- c(
    "*.A.A", "*.A.B", "*.A.C",
    "*.B.A", "*.B.B", "*.B.C",
    "*.C.A", "*.C.B", "*.C.C"
  )
  expect_equal(sort(leaves), sort(expected_leaves))
})
