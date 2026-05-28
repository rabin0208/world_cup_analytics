#!/usr/bin/env Rscript

# Upload data/international/results.csv to GCS, then load it into BigQuery.
# Expected run location: repository root.
#
# R packages: install.packages(c("bigrquery", "googleCloudStorageR", "readr"))
# Run: Rscript src/upload_international_results.R

load_env_from_root <- function() {
  env_path <- ".env"
  if (file.exists(env_path)) {
    readRenviron(env_path)
  }
}

resolve_credential_path <- function(cred_raw) {
  if (startsWith(cred_raw, "/")) {
    return(cred_raw)
  }
  normalizePath(cred_raw, mustWork = TRUE)
}

assert_repo_root <- function() {
  if (!file.exists("world_cup_analytics.Rproj") || !dir.exists("data/international")) {
    stop(
      "Run this script from the repository root. Example:\n",
      "  cd /path/to/world_cup_analytics\n",
      "  Rscript src/upload_international_results.R"
    )
  }
}

main <- function() {
  assert_repo_root()
  load_env_from_root()

  project_id <- Sys.getenv("GCP_PROJECT_ID", "")
  if (!nzchar(project_id)) {
    stop("GCP_PROJECT_ID is not set; add it to .env or the environment.")
  }

  cred_raw <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (!nzchar(cred_raw)) {
    stop(
      "GOOGLE_APPLICATION_CREDENTIALS is not set; ",
      "point it at your service-account JSON."
    )
  }

  cred_path <- resolve_credential_path(cred_raw)
  if (!file.exists(cred_path)) {
    stop("Credential file not found: ", cred_path)
  }
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = cred_path)

  bucket <- "football-analytics-mds496219"
  dataset_id <- "analytics"
  table_id <- "international_results"
  local_csv <- file.path("data", "international", "results.csv")
  if (!file.exists(local_csv)) {
    stop("CSV not found: ", normalizePath(local_csv, mustWork = FALSE))
  }

  date_prefix <- format(Sys.Date(), "%Y-%m-%d")
  gcs_object <- sprintf("raw/international/results/%s/results.csv", date_prefix)
  gcs_uri <- sprintf("gs://%s/%s", bucket, gcs_object)

  suppressPackageStartupMessages({
    library(googleCloudStorageR)
    library(bigrquery)
    library(readr)
  })

  gcs_auth(json_file = cred_path)
  gcs_global_bucket(bucket)
  gcs_upload(local_csv, name = gcs_object)
  message("Uploaded ", normalizePath(local_csv), " -> ", gcs_uri)

  bq_auth(path = cred_path)
  dest <- bq_table(project_id, dataset_id, table_id)
  results <- read_csv(
    local_csv,
    na = c("", "NA"),
    lazy = FALSE,
    show_col_types = FALSE,
    col_types = cols(
      date = col_date(),
      home_team = col_character(),
      away_team = col_character(),
      home_score = col_integer(),
      away_score = col_integer(),
      tournament = col_character(),
      city = col_character(),
      country = col_character(),
      neutral = col_logical()
    )
  )

  bq_table_upload(dest, results, write_disposition = "WRITE_TRUNCATE")
  n_rows <- bq_table_nrow(dest)
  message("Loaded into ", project_id, ".", dataset_id, ".", table_id, " (", n_rows, " rows).")
}

main()
