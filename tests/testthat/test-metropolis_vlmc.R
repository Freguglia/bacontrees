test_that("metropolis_vlmc wrapper works as expected", {
  set.seed(1)
  n <- 150
  result <- metropolis_vlmc(abc_list, n, max_depth = 4)
  expect_setequal(names(result), c("df", "codes", "chain"))
  expect_equal(length(result$chain), 151)
})
