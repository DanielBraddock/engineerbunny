test_that("colnames as expected", {
  column_names <-
    iris |>
    mutate(Species_id = row_number(), .by = Species) |>
    keys_count(Species_id) |>
    as.data.frame() |>
    colnames()
  expect_equal(column_names, c("Species_id", "n", "n_dupes"))
})
