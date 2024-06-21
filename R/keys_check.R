
#' Check columns form a key
#'
#' @description
#' Indicates whether the columns selected as keys truly do form a key
#'
#' ie are sufficient to identify unique and complete rows
#'
#' @details
#' When TRUE it is still possible for a sub-selection to form a key
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a
#' lazy data frame (e.g. from dbplyr or dtplyr).
#' @param keys Selection of columns using tidyselect
#'
#' @return Whether the given variables form a key
#' ie are sufficient to specify uniqueness and completeness. TRUE / FALSE
#' @export
#'
#' @examples \dontrun{
#' iris |> mutate(Species_id = row_number(), .by = Species) |>
#' keys_check(contains("Species")) # TRUE
#' iris |> mutate(id = row_number()) |> keys_check(id) # TRUE
#' iris |> keys_check(everything()) # FALSE
#' iris |> distinct() |> keys_check(everything()) # TRUE
#' iris |> keys_check(c(starts_with("Sepal"), starts_with("Petal"))) # FALSE
#' iris |> keys_check(starts_with("Sepal")) # FALSE
#' iris |> keys_check(Species) # FALSE
#' }
keys_check <- function(df, keys) {
  if (!"tbl_lazy" %in% class(df)) df <- lazy_dt(df)
  n <- df |> count() |> pull(n)
  nd <- df |> count(pick({{ keys }})) |> count() |> pull(n)
  return(n == nd)
}
