set.seed(1)
abc_vec <- sample(letters[1:3], size = 500, replace = TRUE)
abc_list <- lapply(1:3, function(i) sample(letters[1:3], size = 200, replace = TRUE))

usethis::use_data(abc_vec, overwrite = TRUE)
usethis::use_data(abc_list, overwrite = TRUE)
