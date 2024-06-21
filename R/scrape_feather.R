
#' Read all feather files after a pattern into a list of datasets
#'
#' @param as_data_table whether to convert the dataset to data.table
#' @param dir <chr> file path to scrape for .feather files
#' @param pattern <chr> regex to match in file names
#' @param negate <chr> passed to str_detect when selecting files
#'
#' @return named list of datasets scraped from dir, named to match file name, defaults to TRUE
#' @export
#'
#' @examples \dontrun{splits <- scrape_feather("Data")}
scrape_feather <- \(dir, as_data_table = TRUE, pattern = ".*", negate = FALSE) {
  filenames <- list.files(dir) |> str_subset("\\.feather$")
  filenames <- filenames[str_detect(filenames, pattern = pattern, negate = negate)]
  sets <- filenames |> lapply(function(filename) read_feather(file.path(dir, filename)))
  if (as_data_table) sets <- sets |> lapply(as.data.table)
  names(sets) <- filenames |> str_remove(".feather$")
  return(sets)
}
