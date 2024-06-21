test_that("works for data.frame", {
  df <- iris |> mutate(id = row_number())
  expect_equal(as.data.frame(keys_profile(df, id)), data.frame(n_dupes = 0, n = 150))
})

test_that("works for data.table", {
  df <- iris |> as.data.table() |> mutate(id = row_number())
  expect_equal(as.data.frame(keys_profile(df, id)), data.frame(n_dupes = 0, n = 150))
})

test_that("works for lazy_tbl", {
  df <- memdb_frame(id = 1:5)
  expect_equal(as.data.frame(keys_profile(df, id)), data.frame(n_dupes = 0, n = 5))
})

test_that("works for tidyselect with contains()", {
  df <- iris |> mutate(Species_id = row_number(), .by = Species)
  expect_equal(as.data.frame(keys_profile(df, contains("Species"))), data.frame(n_dupes = 0, n = 150))
})
