test_that("keys_check works for data.frame", {
  df <- iris
  df$id <- 1:nrow(df)
  expect_true(keys_check(df, id))
})

test_that("keys_check works for data.table", {
  df <- iris |> as.data.table()
  df$id <- 1:nrow(df)
  expect_true(keys_check(df, id))
})

test_that("keys_check works for lazy_tbl", {
  df <- memdb_frame(id = 1:5)
  expect_true(keys_check(df, id))
})

test_that("keys_check works for multi-id frame", {
  df <- iris
  df$id <- NA_integer_
  df[1:50, "id"] <- 1:50
  df[51:100, "id"] <- 1:50
  df[101:150, "id"] <- 1:50
  expect_true(keys_check(df, c(Species, id)))
})
