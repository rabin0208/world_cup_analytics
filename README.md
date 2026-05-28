# World Cup Analytics

Exploratory dashboard for international football match data (`results.csv` and related CSVs under `data/international/`).

## Run locally

1. Open this folder in RStudio and set the working directory to the project root.

2. Install dependencies:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "DT", "bsicons", "bigrquery", "rsconnect"))
```

3. (Optional) Load `results` from BigQuery instead of the bundled CSV: copy `.env.example` to `.env`, set `GCP_PROJECT_ID`, `GOOGLE_APPLICATION_CREDENTIALS` (path to a service-account JSON with BigQuery read access), and `USE_BIGQUERY=TRUE`.

4. Start the app from the project root (so `.env` and paths resolve):

```r
shiny::runApp("src/app.R")
```

Compatibility launcher (legacy path) is still available at `app.R`.

## Deploy to Posit Connect Cloud

1. Ensure `manifest.json` sits at the repo root. Regenerate it after adding packages or changing bundled files. The repo’s `.rscignore` keeps `.env` and `service-account-key.json` out of the Connect bundle (do not commit secrets).

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "src/app.R")
```

2. Commit and push `src/app.R`, `src/R/`, `manifest.json`, and `data/international/` to GitHub.

3. In [Posit Connect Cloud](https://connect.posit.cloud/), connect the repo and deploy. Connect uses `manifest.json` to restore R packages.

**Note:** By default the app reads `results.csv` from `data/international/` in the repo. With `USE_BIGQUERY=TRUE` and the same GCP env vars on the server, `results` can come from BigQuery instead.

## Data

| File | Description |
|------|-------------|
| `results.csv` | International matches (date, teams, scores, tournament, venue) |
| `shootouts.csv` | Penalty shootout outcomes |
| `former_names.csv` | Historical country name mappings |

Ingestion to GCS/BigQuery: `upload_international_results.R`.

## Dashboard (starter views)

- **Filters:** fixture picker plus competition scope dropdown (all competitions vs FIFA World Cup only)
- **Overview:** matches and goals per year
- **Teams:** most active teams in the selection
- **Results:** searchable match table
- **World Cup:** 2026 unplayed fixtures (72 group-stage matches with no score yet), fixture picker, head-to-head (all competitions + World Cup-only), full schedule table
