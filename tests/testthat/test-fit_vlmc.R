test_that("VLMC fitting via Context Algorithm works", {
  cutoff_value <- 20
  results <- fit_vlmc(abc_list, cutoff = cutoff_value, max_length = 4)
  expect_setequal(results$getActiveNodes(),
                  c("*.a", "*.b", "*.c.a", "*.c.b", "*.c.c"))
  expect_true(results$nodes[["*.a.b"]]$extra$pruneTestStatistic < cutoff_value)
  expect_true(results$nodes[["*.a"]]$extra$pruneTestStatistic > cutoff_value)
})
