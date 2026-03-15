library(dplyr)
library(purrr)
library(arrow)
library(glue)
library(pointblank)

# ---- Expected schemas -------------------------------------------------------

EXPECTED_COLS <- list(
  employment = sort(c(
    "age_bracket", "agency", "agency_code", "agency_subelement",
    "agency_subelement_code", "annualized_adjusted_basic_pay",
    "appointment_type", "appointment_type_code", "bargaining_unit",
    "bargaining_unit_code", "bargaining_unit_status", "cfo_act_agency_indicator",
    "consolidated_statistical_area", "consolidated_statistical_area_code",
    "core_based_statistical_area", "core_based_statistical_area_code", "count",
    "duty_station_code", "duty_station_country", "duty_station_country_code",
    "duty_station_county", "duty_station_county_code", "duty_station_state",
    "duty_station_state_abbreviation", "duty_station_state_code",
    "duty_station_state_country_territory_code", "education_level",
    "education_level_bracket", "education_level_code", "flsa_category",
    "flsa_category_code", "grade", "length_of_service_years",
    "locality_pay_area", "locality_pay_area_code", "nsftp_indicator",
    "occupational_category", "occupational_category_code", "occupational_group",
    "occupational_group_code", "occupational_series", "occupational_series_code",
    "pay_basis", "pay_basis_code", "pay_plan", "pay_plan_code",
    "personnel_office_identifier_code", "position_occupied",
    "position_occupied_code", "service_computation_date_leave",
    "snapshot_yyyymm", "stem_occupation", "stem_occupation_type",
    "step_or_rate_type", "step_or_rate_type_code", "supervisory_status",
    "supervisory_status_code", "tenure", "tenure_code", "veteran_indicator",
    "work_schedule", "work_schedule_code"
  )),
  accessions = sort(c(
    "accession_category", "accession_category_code", "age_bracket", "agency",
    "agency_code", "agency_subelement", "agency_subelement_code",
    "annualized_adjusted_basic_pay", "appointment_not_to_exceed_date",
    "appointment_type", "appointment_type_code", "bargaining_unit",
    "bargaining_unit_code", "bargaining_unit_status", "cfo_act_agency_indicator",
    "consolidated_statistical_area", "consolidated_statistical_area_code",
    "core_based_statistical_area", "core_based_statistical_area_code", "count",
    "duty_station_code", "duty_station_country", "duty_station_country_code",
    "duty_station_county", "duty_station_county_code", "duty_station_state",
    "duty_station_state_abbreviation", "duty_station_state_code",
    "duty_station_state_country_territory_code", "education_level",
    "education_level_bracket", "education_level_code", "flsa_category",
    "flsa_category_code", "grade", "length_of_service_years",
    "locality_pay_area", "locality_pay_area_code", "nsftp_indicator",
    "occupational_category", "occupational_category_code", "occupational_group",
    "occupational_group_code", "occupational_series", "occupational_series_code",
    "pay_basis", "pay_basis_code", "pay_plan", "pay_plan_code",
    "personnel_action_effective_date_yyyymm", "personnel_office_identifier_code",
    "position_occupied", "position_occupied_code", "service_computation_date_leave",
    "stem_occupation", "stem_occupation_type", "step_or_rate_type",
    "step_or_rate_type_code", "supervisory_status", "supervisory_status_code",
    "tenure", "tenure_code", "veteran_indicator", "work_schedule",
    "work_schedule_code"
  )),
  separations = sort(c(
    "age_bracket", "agency", "agency_code", "agency_subelement",
    "agency_subelement_code", "annualized_adjusted_basic_pay",
    "appointment_not_to_exceed_date", "appointment_type", "appointment_type_code",
    "bargaining_unit", "bargaining_unit_code", "bargaining_unit_status",
    "cfo_act_agency_indicator", "consolidated_statistical_area",
    "consolidated_statistical_area_code", "core_based_statistical_area",
    "core_based_statistical_area_code", "count", "drp_indicator",
    "duty_station_code", "duty_station_country", "duty_station_country_code",
    "duty_station_county", "duty_station_county_code", "duty_station_state",
    "duty_station_state_abbreviation", "duty_station_state_code",
    "duty_station_state_country_territory_code", "education_level",
    "education_level_bracket", "education_level_code", "flsa_category",
    "flsa_category_code", "grade", "length_of_service_years",
    "locality_pay_area", "locality_pay_area_code", "nsftp_indicator",
    "occupational_category", "occupational_category_code", "occupational_group",
    "occupational_group_code", "occupational_series", "occupational_series_code",
    "pay_basis", "pay_basis_code", "pay_plan", "pay_plan_code",
    "personnel_action_effective_date_yyyymm", "personnel_office_identifier_code",
    "position_occupied", "position_occupied_code", "separation_category",
    "separation_category_code", "service_computation_date_leave", "stem_occupation",
    "stem_occupation_type", "step_or_rate_type", "step_or_rate_type_code",
    "supervisory_status", "supervisory_status_code", "tenure", "tenure_code",
    "veteran_indicator", "work_schedule", "work_schedule_code"
  ))
)

DHS_AGENCY  <- "DEPARTMENT OF HOMELAND SECURITY"
DATA_TYPES  <- names(EXPECTED_COLS)
# OPM typically publishes data with a ~2 month lag; allow up to 3 months
MAX_LAG_MONTHS <- 3

# ---- Helpers ----------------------------------------------------------------

# Parse YYYYMM string from filename like "employment-dhs-202601.feather"
yyyymm_from_path <- function(path) {
  sub(".*-(\\d{6})\\.feather$", "\\1", basename(path))
}

# All expected YYYYMM strings from 2021-01 through a given date
expected_months <- function(through_date = Sys.Date()) {
  seq(
    as.Date("2021-01-01"),
    as.Date(format(through_date, "%Y-%m-01")),
    by = "month"
  ) |>
    format("%Y%m")
}

# Row count from a feather file without loading all columns
row_count <- function(path) nrow(read_feather(path))

# ---- Per-type checks --------------------------------------------------------

check_type <- function(type, today = Sys.Date()) {
  files      <- sort(list.files("data", pattern = glue("{type}-dhs-\\d{{6}}\\.feather"), full.names = TRUE))
  months_on_disk <- yyyymm_from_path(files)

  results <- list()

  # 1. Any files exist at all
  results$has_files <- tibble(
    check  = "files exist",
    type   = type,
    status = if (length(files) > 0) "PASS" else "FAIL",
    detail = glue("{length(files)} file(s) found")
  )

  if (length(files) == 0) return(bind_rows(results))

  # 2. Most recent month is within MAX_LAG_MONTHS of today
  latest_yyyymm <- tail(months_on_disk, 1)
  latest_date   <- as.Date(paste0(latest_yyyymm, "01"), "%Y%m%d")
  lag_months    <- as.integer(
    (as.integer(format(today, "%Y")) - as.integer(format(latest_date, "%Y"))) * 12 +
      (as.integer(format(today, "%m")) - as.integer(format(latest_date, "%m")))
  )
  results$recency <- tibble(
    check  = "recency: latest month within lag window",
    type   = type,
    status = if (lag_months <= MAX_LAG_MONTHS) "PASS" else "WARN",
    detail = glue("latest = {latest_yyyymm}, lag = {lag_months} month(s) (max {MAX_LAG_MONTHS})")
  )

  # 3. No gaps in the monthly series
  # Only check up to the latest month we have (OPM may not have published further)
  expected <- expected_months(latest_date)
  missing  <- setdiff(expected, months_on_disk)
  results$no_gaps <- tibble(
    check  = "no gaps in monthly series",
    type   = type,
    status = if (length(missing) == 0) "PASS" else "FAIL",
    detail = if (length(missing) == 0) glue("all {length(expected)} months present")
             else glue("missing: {paste(missing, collapse = ', ')}")
  )

  # ---- Load latest file for content checks ----------------------------------
  latest_path <- tail(files, 1)
  df <- read_feather(latest_path)

  # 4. Column count
  expected_n <- length(EXPECTED_COLS[[type]])
  actual_n   <- ncol(df)
  results$col_count <- tibble(
    check  = "column count",
    type   = type,
    status = if (actual_n == expected_n) "PASS" else "FAIL",
    detail = glue("expected {expected_n}, got {actual_n}")
  )

  # 5. No missing expected columns
  missing_cols <- setdiff(EXPECTED_COLS[[type]], names(df))
  results$col_names_missing <- tibble(
    check  = "no missing columns",
    type   = type,
    status = if (length(missing_cols) == 0) "PASS" else "FAIL",
    detail = if (length(missing_cols) == 0) "all expected columns present"
             else glue("missing: {paste(missing_cols, collapse = ', ')}")
  )

  # 6. No unexpected extra columns
  extra_cols <- setdiff(names(df), EXPECTED_COLS[[type]])
  results$col_names_extra <- tibble(
    check  = "no unexpected columns",
    type   = type,
    status = if (length(extra_cols) == 0) "PASS" else "WARN",
    detail = if (length(extra_cols) == 0) "no extra columns"
             else glue("extra: {paste(extra_cols, collapse = ', ')}")
  )

  # 7. All rows are character type (as expected from col_types = "c" on read)
  non_char <- names(df)[!sapply(df, is.character)]
  results$all_character <- tibble(
    check  = "all columns are character type",
    type   = type,
    status = if (length(non_char) == 0) "PASS" else "WARN",
    detail = if (length(non_char) == 0) "all character"
             else glue("non-character: {paste(non_char, collapse = ', ')}")
  )

  # ---- pointblank agent for content rules -----------------------------------
  al <- action_levels(warn_at = 0.0001, stop_at = 0.01)

  date_label <- if (type == "employment") {
    glue("snapshot_yyyymm == {latest_yyyymm}")
  } else {
    "personnel_action_effective_date_yyyymm is YYYYMM"
  }
  date_col   <- if (type == "employment") "snapshot_yyyymm" else "personnel_action_effective_date_yyyymm"
  date_regex <- if (type == "employment") glue("^{latest_yyyymm}$") else "^\\d{6}$"

  agent <- df |>
    create_agent(
      label   = glue("OPM DHS {type} — {latest_yyyymm}"),
      actions = al
    ) |>
    # 8. agency column always == DHS
    col_vals_equal(
      columns = vars(agency),
      value   = DHS_AGENCY,
      label   = "agency == DHS"
    ) |>
    # 9. count not null
    col_vals_not_null(
      columns = vars(count),
      label   = "count not null"
    ) |>
    # 10. count is a positive integer string (regex catches non-numeric and zero)
    col_vals_regex(
      columns = vars(count),
      regex   = "^[1-9]\\d*$",
      label   = "count is positive integer string"
    ) |>
    # 11. agency_code not null
    col_vals_not_null(
      columns = vars(agency_code),
      label   = "agency_code not null"
    ) |>
    # 12. date field: exact match for employment snapshot, regex for flow types
    col_vals_regex(
      columns = all_of(date_col),
      regex   = date_regex,
      label   = date_label
    ) |>
    interrogate()

  pb_summary <- agent$validation_set |>
    transmute(
      check  = label,
      type   = type,
      status = case_when(
        !all_passed ~ "FAIL",
        warn        ~ "WARN",
        TRUE        ~ "PASS"
      ),
      detail = glue("rows checked: {format(n, big.mark=',')}; failing: {format(n_failed, big.mark=',')}")
    )

  results$pointblank <- pb_summary

  # 13. Row count plausibility vs. prior 3 months (WARN only)
  prior_files <- files[months_on_disk < latest_yyyymm]
  if (length(prior_files) >= 3) {
    prior_counts <- map_int(tail(prior_files, 3), row_count)
    avg_prior    <- mean(prior_counts)
    cur_count    <- nrow(df)
    ratio        <- cur_count / avg_prior
    results$row_count_plausible <- tibble(
      check  = "row count plausible vs. prior 3 months",
      type   = type,
      status = if (ratio >= 0.25 & ratio <= 4) "PASS" else "WARN",
      detail = glue(
        "current = {format(cur_count, big.mark=',')}, ",
        "3-month avg = {format(round(avg_prior), big.mark=',')}, ",
        "ratio = {round(ratio, 2)}"
      )
    )
  }

  bind_rows(results)
}

# ---- Run all checks ---------------------------------------------------------

today   <- Sys.Date()
results <- map(DATA_TYPES, check_type, today = today) |> bind_rows()

# ---- Pretty print -----------------------------------------------------------

status_icon <- function(s) {
  switch(s, PASS = cli::col_green("PASS"), WARN = cli::col_yellow("WARN"), FAIL = cli::col_red("FAIL"), s)
}

cat("\n")
cat(cli::rule(left = glue("OPM DHS data checks — {today}"), width = 70), "\n\n")

results |>
  mutate(icon = map_chr(status, status_icon)) |>
  pwalk(function(check, type, status, detail, icon, ...) {
    cat(glue("  [{icon}]  {formatC(type, width=12, flag='-')} {check}\n"))
    if (nchar(detail) > 0 && status != "PASS") {
      cat(glue("            {cli::col_silver(detail)}\n"))
    }
  })

cat("\n")
summary_line <- results |>
  count(status) |>
  mutate(label = glue("{n} {status}")) |>
  pull(label) |>
  paste(collapse = " | ")

cat(cli::rule(width = 70), "\n")
cat(glue("  {summary_line}\n\n"))

# ---- Fail fast for CI -------------------------------------------------------

n_fail <- sum(results$status == "FAIL")
if (n_fail > 0) {
  stop(glue("{n_fail} check(s) FAILED — see output above."), call. = FALSE)
}
