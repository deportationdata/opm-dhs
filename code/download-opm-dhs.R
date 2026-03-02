library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(httr2)
library(arrow)
library(glue)

# ---- Configuration ----

DATA_TYPES <- c("accessions", "separations", "employment")
DHS_AGENCY <- "DEPARTMENT OF HOMELAND SECURITY"
BASE_URL <- "https://data.opm.gov/api/blob/download/chunked"
MAX_VERSIONS <- 5

# ---- Helpers ----

opm_url <- function(type, year, month, version) {
  glue("{BASE_URL}/{type}_{year}{sprintf('%02d', month)}_{version}.txt")
}

out_path <- function(type, year, month) {
  glue("data/{type}-dhs-{year}{sprintf('%02d', month)}.feather")
}

# Try one version of a monthly file; returns temp file path or NULL
try_download <- function(url) {
  tmp <- tempfile(fileext = ".txt")

  resp <- request(url) |>
    req_timeout(600) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform(path = tmp)

  if (resp_status(resp) != 200) {
    unlink(tmp)
    return(NULL)
  }

  tmp
}

# Download the highest available version for a month; returns temp path or NULL
download_month <- function(type, year, month) {
  best <- NULL

  for (v in seq_len(MAX_VERSIONS)) {
    path <- try_download(opm_url(type, year, month, v))
    if (is.null(path)) {
      break
    }
    if (!is.null(best)) {
      unlink(best)
    }
    best <- path
  }

  best
}

# Read pipe-delimited file, filter to DHS, write feather
filter_and_save <- function(tmp_path, type, year, month) {
  df <- read_delim(
    tmp_path,
    delim = "|",
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )

  dhs <- df |>
    filter(agency == DHS_AGENCY)

  path <- out_path(type, year, month)
  arrow::write_feather(dhs, path)

  message(sprintf(
    "  Saved %s (%d DHS rows / %d total)",
    path,
    nrow(dhs),
    nrow(df)
  ))
}

# ---- Main ----

dir.create("data", showWarnings = FALSE, recursive = TRUE)

params <- crossing(
  type = DATA_TYPES,
  year = 2021:as.integer(format(Sys.Date(), "%Y")),
  month = 1:12
) |>
  # don't attempt future months
  filter(as.Date(sprintf("%d-%02d-01", year, month)) <= Sys.Date()) |>
  # skip months already downloaded
  filter(!file.exists(out_path(type, year, month)))

message(sprintf("Processing %d month/type combinations", nrow(params)))

pwalk(params, function(type, year, month) {
  message(sprintf("\n%s %d-%02d", type, year, month))

  tmp <- download_month(type, year, month)

  if (is.null(tmp)) {
    message("  Not yet available on OPM")
    return(invisible(NULL))
  }

  filter_and_save(tmp, type, year, month)
  unlink(tmp)
})
