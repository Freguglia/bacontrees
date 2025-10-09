test_that("TreeNode initialization works correctly", {
  node <- TreeNode$new(path = "*.0")
  expect_equal(node$getPath(), "*.0")
  expect_equal(node$getDepth(), 1)
  expect_equal(node$getParentPath(), "*")
})

test_that("TreeNode print method works correctly", {
  node <- TreeNode$new(path = "*.0")
  expect_output(node$print(), "\\*.0")
})

test_that("TreeNode activation and deactivation works correctly", {
  node <- TreeNode$new(path = "*.0")
  expect_false(node$isActive())
  node$activate()
  expect_true(node$isActive())
  node$deactivate()
  expect_false(node$isActive())
})

test_that("TreeNode validatePath works correctly", {
  alphabet <- list(symbols = c("*", "0", "1", "2"))
  node <- TreeNode$new(path = "*.0")
  expect_true(node$validatePath(alphabet))

  node_invalid <- TreeNode$new(path = "*.invalid")
  expect_false(node_invalid$validatePath(alphabet))
})

test_that("TreeNode depth calculation is correct", {
  node <- TreeNode$new(path = "*.0.1")
  expect_equal(node$getDepth(), 2)
  expect_equal(node$getParentPath(), "*.0")
})

test_that("TreeNode handles root path correctly", {
  node <- TreeNode$new(path = "*")
  expect_equal(node$getPath(), "*")
  expect_equal(node$getDepth(), 0)
  expect_true(is.na(node$getParentPath()))
})

test_that("TreeNode handles multiple levels correctly", {
  node <- TreeNode$new(path = "*.0.1.2")
  expect_equal(node$getPath(), "*.0.1.2")
  expect_equal(node$getDepth(), 3)
  expect_equal(node$getParentPath(), "*.0.1")
})
