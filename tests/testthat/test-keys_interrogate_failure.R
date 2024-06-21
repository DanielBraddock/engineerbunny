test_that("works for column passed as quoted expression", {
  expect_no_error(iris |> keys_interrogate_failure("Species", dupe_severity = 49))
})

test_that("column names as expected", {
  comparison <- iris |> keys_interrogate_failure("Species", dupe_severity = 49)
  expect_true("variable" %in% names(comparison))
  expect_true("id_1" %in% names(comparison))
  expect_true("id_2" %in% names(comparison))
  expect_true("id_50" %in% names(comparison))
})

test_that("works for tidyselection", {
  k <- c("Species", "Sepal.Length", "Sepal.Width")
  expect_no_error(iris |> keys_interrogate_failure(all_of(k), dupe_severity = 2))
})
