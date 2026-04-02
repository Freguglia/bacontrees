test_that("metropolis_vlmc wrapper works as expected", {
  set.seed(1)
  n <- 150
  result <- metropolis_vlmc(abc_list[1:2], n, max_depth = 4)
  expect_setequal(names(result), c("df", "codes", "chain", "baConTree"))
  expect_equal(length(result$chain), 151)

  result2 <- metropolis_vlmc(abc_vec, n, max_depth = 4)
  expect_setequal(names(result2), c("df", "codes", "chain", "baConTree"))
  expect_equal(length(result2$chain), 151)
})

test_that("metropolis_vlmc df has correct columns and valid probabilities", {
  set.seed(1)
  result <- metropolis_vlmc(abc_vec, n_steps = 200, max_depth = 3, burnin = 50)
  expect_setequal(names(result$df), c("tree_contexts", "prob", "n", "tree_code"))
  expect_equal(sum(result$df$prob), 1)
  expect_true(all(result$df$prob > 0))
})

test_that("print.metropolis_vlmc works without error", {
  set.seed(1)
  result <- metropolis_vlmc(abc_vec, n_steps = 200, max_depth = 3)
  expect_output(print(result))
})
