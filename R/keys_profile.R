
#' Count severity of duplication using a selection of columns
#'
#' Profile the different levels (or severity) of dupes (0, 1, 2, ... dupes)
#' present in the data whereby uniqueness is defined by the "keys"
#'
#' keys really are keys if the resulting tibble has one row. This row shows
#' there are n (= nrow(df)) unique combinations of the levels of the keys
#' with 0 dupes each.
#'
#' Otherwise the resulting tibble has multiple rows. Each row shows how many of
#' each unique combination of the keys have 0 dupes, 1 dupe, 2 dupes etc. Here,
#' the "how many" bit is the profile and the "0 dupes" bit is the severity.
#' In this case, keys isn't really quite the right word: as even a single dupe
#' proves these keys aren't really keys at all
#'
#' @inheritParams keys_check
#'
#' @return A data.frame, or dtplyr_step_mutate, of results profiling how many levels of the keys have
#' 0 dupes, 1 dupe, 2 dupes and so on
#' @export
#'
#' @examples \dontrun{
#' iris |> mutate(Species_id = row_number(), .by = Species) |>
#' keys_profile_dupe_severity(contains("Species"))
#' iris |> mutate(id = row_number()) |> keys_profile_dupe_severity(id)
#' iris |> keys_profile_dupe_severity(everything())
#' iris |> keys_profile_dupe_severity(c(starts_with("Sepal"), starts_with("Petal")))
#' iris |> keys_profile_dupe_severity(starts_with("Sepal"))
#' iris |> keys_profile_dupe_severity(Species)
#' }
keys_profile <- function(df, keys) {
  if (!"tbl_lazy" %in% class(df)) df <- lazy_dt(df)
  result <- df |> count(pick({{ keys }})) |> mutate(n_dupes = .data$n - 1)
  result <- result |> count(.data$n_dupes) |> arrange(.data$n_dupes)
  return(result)
}
