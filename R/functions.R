#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows Maximum number of rows to read.
#'
#' @returns Outputs a data frame/tibble.
#'
read <- function(file_path, max_rows = 100) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}


#' Read all `.csv.gz` files in the `stress/` folder into one data frame.
#'
#' @param filename The name of files in the sub-folders that we
#'    want to read in.
#'
#' @returns A single data frame/tibble.

read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)


  data <- files |>
    purrr::map(read) |>
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}

read_all("HR.csv.gz")
read_all("IBI.csv.gz")


