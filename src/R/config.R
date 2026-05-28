get_project_root <- function() {
  candidates <- c(
    normalizePath(getwd(), mustWork = TRUE),
    normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
  )
  candidates <- unique(candidates[dir.exists(candidates)])

  for (candidate in candidates) {
    if (dir.exists(file.path(candidate, "data", "international"))) {
      return(candidate)
    }
  }

  stop("Expected data at data/international/ (run from project root).")
}

env_is_true <- function(x) {
  v <- toupper(trimws(x))
  nzchar(v) && v %in% c("1", "TRUE", "YES", "Y")
}

load_results <- function(data_dir, project_root) {
  csv_path <- file.path(data_dir, "results.csv")
  if (!env_is_true(Sys.getenv("USE_BIGQUERY", ""))) {
    message("Loading results from CSV (set USE_BIGQUERY=TRUE in .env to use BigQuery).")
    return(
      read.csv(
        csv_path,
        stringsAsFactors = FALSE,
        na.strings = c("", "NA")
      )
    )
  }

  project_id <- Sys.getenv("GCP_PROJECT_ID", "")
  cred_raw <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (!nzchar(project_id)) {
    stop("USE_BIGQUERY=TRUE requires GCP_PROJECT_ID in .env")
  }
  if (!nzchar(cred_raw)) {
    stop("USE_BIGQUERY=TRUE requires GOOGLE_APPLICATION_CREDENTIALS in .env")
  }

  cred_path <- if (startsWith(cred_raw, "/")) {
    cred_raw
  } else {
    normalizePath(file.path(project_root, cred_raw), mustWork = TRUE)
  }
  if (!file.exists(cred_path)) {
    stop("Credential file not found: ", cred_path)
  }

  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = cred_path)
  bq_auth(path = cred_path)

  dataset_id <- Sys.getenv("GCP_BQ_DATASET", "analytics")
  table_id <- Sys.getenv("GCP_BQ_RESULTS_TABLE", "international_results")
  dest <- bq_table(project_id, dataset_id, table_id)
  message(
    "Loading results from BigQuery: ",
    project_id, ".", dataset_id, ".", table_id
  )

  bq_table_download(dest, bigint = "integer")
}

prepare_results <- function(results) {
  results$date <- as.Date(results$date)
  results$year <- as.integer(format(results$date, "%Y"))
  results$total_goals <- results$home_score + results$away_score
  results$neutral <- as.logical(results$neutral)
  results
}
