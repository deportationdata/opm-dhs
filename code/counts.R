library(dplyr)
library(purrr)
library(arrow)

employment_df <-
  list.files(
    "data",
    pattern = "^employment-dhs-\\d{6}\\.feather$",
    full.names = TRUE
  ) |>
  set_names() |>
  map_dfr(read_feather, .id = "file") |>
  group_by(agency_subelement, file) |>
  summarise(n_employees = sum(as.integer(count)), .groups = "drop") |>
  mutate(
    date_part = sub(".*-(\\d{6})\\.feather$", "\\1", basename(file)),
    year = as.integer(substr(date_part, 1, 4)),
    month = as.integer(substr(date_part, 5, 6)),
    .keep = "unused"
  )

arrow::write_feather(
  employment_df,
  "data/opm-employee-counts-dhs.feather"
)
