library(dplyr)
library(purrr)
library(arrow)
library(stringr)

get_subagency_count <- function(path) {
  read_feather(path) |>
    count(agency_subelement)
}

employment_df <-
  list.files(
    "data",
    pattern = "^employment-dhs-\\d{6}\\.feather$",
    full.names = TRUE
  ) |>
  set_names() |>
  map_dfr(read_feather, .id = "file") |>
  count(agency_subelement, file, name = "n_employees") |>
  mutate(
    date_part = str_extract(file, "\\d{6}(?=\\.feather)"),
    .keep = "unused"
  ) |>
  mutate(
    year = as.integer(str_sub(date_part, 1, 4)),
    month = as.integer(str_sub(date_part, 5, 6)),
    .keep = "unused"
  )

arrow::write_feather(
  employment_df,
  "data/opm-employee-counts-dhs.feather"
)
