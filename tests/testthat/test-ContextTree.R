test_that("Initialization works correctly", {
  alphabet <- LETTERS[1:3]
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

  # nodes is read-only; structural mutation must be rejected
  expect_error(tree$nodes[["*.D"]] <- TreeNode$new("*.D"))
})

test_that("Active nodes are correctly identified when growing or pruning", {
  alphabet <- LETTERS[1:3]
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
  tree <- ContextTree$new(alphabet = LETTERS[1:3], maximalDepth = 2)
  tree$activateRoot()

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
  tree <- ContextTree$new(alphabet = LETTERS[1:3], maximalDepth = 2)
  tree$activateRoot()

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

test_that("Tree Compression works as expected", {
  tree <- ContextTree$new(alphabet = LETTERS[1:3], maximalDepth = 4)
  tree$activateRoot()
  code_root <- tree$activeTreeCode()
  tree$growActive("*")
  tree$growActive("*.A")
  tree$growActive("*.A.C")

  code <- tree$activeTreeCode()
  active_node_vector <- unname(map_lgl(tree$nodes, ~.x$isActive()))
  expect_setequal(map_chr(tree$nodes[active_node_vector], ~.x$getPath()),
                  tree$getActiveNodes())
  expect_equal(active_node_vector, decompress_logical(code, length(tree$nodes)))

  tree$activateRoot()
  expect_equal(tree$activeTreeCode(), code_root)

  tree$activateByCode(code)
  expect_setequal(tree$getActiveNodes(), c("*.A.C.A", "*.A.C.B", "*.A.C.C",
                                           "*.A.B", "*.A.A", "*.B", "*.C"))
})

test_that("Inner nodes are correctly obtained", {
  tree <- ContextTree$new(alphabet = LETTERS[1:2], maximalDepth = 4)
  tree$activateRoot()
  expect_length(tree$getInnerNodes(), 0)
  tree$growActive("*")
  expect_length(tree$getInnerNodes(), 1)
  expect_setequal(tree$getInnerNodes(), "*")
  tree$growActive("*.A")
  expect_length(tree$getInnerNodes(), 2)
  expect_setequal(tree$getInnerNodes(), c("*", "*.A"))
})

test_that("activateMaximal sets exactly the maximal leaves as active", {
  tree <- ContextTree$new(alphabet = LETTERS[1:2], maximalDepth = 2)
  tree$activateMaximal()
  expect_setequal(tree$getActiveNodes(), tree$getLeaves())
})

test_that("Initialization from data infers alphabet and fills counts", {
  tree <- ContextTree$new(abc_vec, maximalDepth = 2)
  expect_setequal(tree$getAlphabet()$symbols, c("a", "b", "c"))
  expect_equal(tree$getMaximalDepth(), 2)
  expect_equal(sum(tree$root()$counts), length(abc_vec) - 2)
})

test_that("getDataset returns the attached Sequence object", {
  tree <- ContextTree$new(abc_vec, maximalDepth = 2)
  ds <- tree$getDataset()
  expect_true(inherits(ds, "Sequence"))
})

test_that("setData errors if called a second time", {
  tree <- ContextTree$new(abc_vec, maximalDepth = 2)
  expect_error(tree$setData(abc_vec), regexp = "already has data")
})

test_that("Initialization errors when alphabet is incompatible with data", {
  bad_alphabet <- Alphabet$new(c("x", "y", "z"))
  expect_error(ContextTree$new(abc_vec, maximalDepth = 2, alphabet = bad_alphabet))
})

