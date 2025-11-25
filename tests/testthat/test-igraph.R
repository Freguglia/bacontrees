test_that("conversion to igraph works", {
  ct <- ContextTree$new(abc_list)
  ct_ig <- ct$igraph(activeOnly = FALSE)

  expect_true("igraph" %in% class(ct_ig))

  expect_length(igraph::V(ct_ig)[["*"]]$p, 3)
  expect_length(igraph::V(ct_ig)[["*"]]$n, 1)
  expect_length(igraph::V(ct_ig)[["*"]]$foo, 0)
  expect_equal(igraph::V(ct_ig)[["*.a.a.b"]]$nodeLabel, "b")
  expect_length(igraph::E(ct_ig)[[3]]$n, 1)
})
