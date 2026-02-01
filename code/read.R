library(duckplyr)
library(dplyr)
library(tidyverse)

# Enable HTTPS reads
# duckplyr::db_exec("INSTALL httpfs")
# duckplyr::db_exec("LOAD httpfs")

get_subagency_count <- function(yr, mo) {
  url <- glue::glue(
    "https://huggingface.co/datasets/abigailhaddad/opm-federal-employment-{yr}{sprintf('%02d', mo)}/resolve/main/data.parquet"
  )

  d <- duckplyr::read_parquet_duckdb(url)

  # # Example: keep it lazy/duckdb-backed until you pull results
  d |>
    filter(agency == "DEPARTMENT OF HOMELAND SECURITY") |>
    count(agency_subelement) |>
    as_tibble() |>
    mutate(year = yr, month = mo)
}

params <- crossing(yr = 2021:2025, mo = 1:12) |>
  filter(!(yr == 2025 & mo > 11))

result <- pmap_dfr(params, ~ get_subagency_count(..1, ..2))

arrow::write_feather(
  result,
  "data/opm-employee-counts-dhs.feather"
)
