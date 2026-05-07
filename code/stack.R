library(dplyr)
library(purrr)
library(stringr)
library(arrow)
library(haven)
library(writexl)

for (type in c("accessions", "employment", "separations")) {
  sheet_label <- str_to_title(type)

  stacked <-
    list.files(
      "slices",
      pattern = paste0("^", type, "-immigration-\\d{6}\\.parquet$"),
      full.names = TRUE
    ) |>
    map_dfr(\(path) {
      date_part <- sub(".*-(\\d{6})\\.parquet$", "\\1", basename(path))
      read_parquet(path) |>
        mutate(
          year = as.integer(substr(date_part, 1, 4)),
          month = as.integer(substr(date_part, 5, 6))
        )
    }) |>
    arrange(
      pick(any_of(c(
        "agency_subelement_code",
        "occupational_series_code",
        "duty_station_code"
      ))),
      year,
      month
    )

  base <- paste0("data/opm-", type, "-immigration")

  arrow::write_parquet(
    stacked,
    paste0(base, ".parquet"),
    compression = "zstd",
    compression_level = 22,
    use_dictionary = TRUE
  )

  stacked |>
    rename_with(
      ~ make.unique(
        abbreviate(.x, minlength = 32, strict = FALSE),
        sep = "_"
      )
    ) |>
    haven::write_dta(paste0(base, ".dta"))

  stacked |>
    mutate(.chunk = ceiling(row_number() / 1e6)) |>
    group_split(.chunk, .keep = FALSE) |>
    set_names(~ str_c(sheet_label, " (Sheet ", seq_along(.x), ")")) |>
    writexl::write_xlsx(paste0(base, ".xlsx"))
}
