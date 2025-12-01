test_that("metropolis_vlmc wrapper works as expected", {
  set.seed(1)
  n <- 150
  result <- metropolis_vlmc(abc_list[1:2], n, max_depth = 4)
  expect_setequal(names(result), c("df", "codes", "chain"))
  expect_equal(length(result$chain), 151)

  result2 <- metropolis_vlmc(abc_vec, n, max_depth = 4, initial_root = FALSE)
  expect_setequal(names(result2), c("df", "codes", "chain", "baConTree"))
  expect_equal(length(result2$chain), 151)
})
