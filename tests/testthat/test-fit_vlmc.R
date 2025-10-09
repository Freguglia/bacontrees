test_that("VLMC fitting via Context Algorithm works for list input", {
  cutoff_value <- 20
  results <- fit_vlmc(abc_list, cutoff = cutoff_value, max_length = 4)
  expect_s3_class(results, "ContextTree")
  expect_setequal(results$getActiveNodes(),
                  c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c"))
  expect_true(results$nodes[["*.a.b"]]$extra$pruneTestStatistic < cutoff_value)
  expect_true(results$nodes[["*.a"]]$extra$pruneTestStatistic > cutoff_value)
  expect_false("*.a.b" %in% results$getActiveNodes())
})

test_that("VLMC fitting works for vector input", {
  cutoff_value <- 20
  results <- fit_vlmc(abc_vec, cutoff = cutoff_value, max_length = 4)
  expect_s3_class(results, "ContextTree")
  expect_true(length(results$getActiveNodes()) > 0)
})

test_that("VLMC fitting handles small data and extreme cutoffs", {
  small_vec <- abc_vec[1:10]
  results <- fit_vlmc(small_vec, cutoff = 0, max_length = 2)
  expect_s3_class(results, "ContextTree")
  expect_true(length(results$getActiveNodes()) > 0)

  results2 <- fit_vlmc(small_vec, cutoff = 1e6, max_length = 2)
  expect_s3_class(results2, "ContextTree")
  expect_true(length(results2$getActiveNodes()) >= 1)
})
