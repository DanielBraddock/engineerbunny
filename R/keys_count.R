
#' Count the times each unique key level appears and hence number of dupes
#'
#' @inheritParams keys_check
#'
#' @return A data.frame, or dtplyr_step_mutate, of results listing the unique levels of the keys and counts
#' @export
#'
#' @examples \dontrun{
#' iris |> mutate(Species_id = row_number(), .by = Species) |> keys_count(Species_id)
#' iris |> mutate(id = row_number()) |> keys_count(id)
#' }
keys_count <- function(df, keys) {
  if (!"tbl_lazy" %in% class(df)) df <- lazy_dt(df)
  result <- df |> count(pick({{ keys }})) |> mutate(n_dupes = .data$n - 1)
  return(result)
}
