#' Find and compare an example of duplication
#'
#' @inheritParams keys_check
#' @param keys Selection of columns using:
#' * tidyselect
#' * names (string) ie a character vector
#'
#' Not like the other keys_*() functions, not using:
#' * unquoted expressions
#' @param dupe_severity The level of duplication required for the comparison
#' @param dupe_id Each example is given an id. Change this for a different example.
#'
#' @return A data.frame listing the columns which ideally would form a key and
#' the columns which are responsible for that key not being a key after all
#' @export
#'
#' @examples \dontrun{
#' iris |> mutate(x = row_number()) |> keys_interrogate_failure(starts_with("Sepal"))
#' iris |> mutate(x = row_number()) |> keys_interrogate_failure(c("Sepal.Length"
#' , "Sepal.Width", "Petal.Length"))
#' iris |> mutate(x = row_number()) |> keys_interrogate_failure(c("Sepal.Length"
#' , "Sepal.Width", "Petal.Length", "Petal.Width"))
#' try(iris |> mutate(x = row_number()) |> keys_interrogate_failure(c("Sepal.Length"
#' , "Sepal.Width", "Petal.Length", "Petal.Width", "x")))
#' }
keys_interrogate_failure <- function(df, keys, dupe_severity = 1, dupe_id = 1) {
  if (!"tbl_lazy" %in% class(df)) df <- lazy_dt(df)
  unique_dupes <- df |>
    count(across(all_of(keys))) |>
    mutate(n_dupes = .data$n - 1) |>
    filter(.data$n_dupes > 0)
  nd <- unique_dupes |> count() |> pull("n")
  if (nd == 0) stop("no dupes")
  unique_dupes <- unique_dupes |> filter(.data$n_dupes == dupe_severity)
  unique_dupe <- unique_dupes |> slice(dupe_id)
  dupes_wide <- df |> semi_join(unique_dupe)
  dupes_wide <- dupes_wide |> mutate(id = paste0("id_", row_number()))
  dupes_wide <- dupes_wide |> mutate(across(everything(), as.character))
  dupes_superlong <- dupes_wide |>
    pivot_longer(cols = -"id"
                 , names_to = "variable"
                 , values_to = "observation")
  dupes_long <- dupes_superlong |>
    pivot_wider(names_from = "id"
                , values_from = "observation")
  dupes_long <- dupes_long |> mutate(match_1_2 = .data$id_1 == .data$id_2 | all(is.na(.data$id_1), is.na(.data$id_2))) # allows for (NA, NA)
  dupes_long <- dupes_long |> filter(.data$variable %in% keys | !.data$match_1_2)
  return(as.data.frame(dupes_long))
}
