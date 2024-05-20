set.seed(1)
bacon_indep <- sample(c("a", "b", "c", "n", "o"), size = 200, replace = TRUE)

usethis::use_data(bacon_indep, overwrite = TRUE)
