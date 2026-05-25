# World Cup Analytics

Exploratory dashboard for international football match data (results, goalscorers, and related CSVs under `data/international/`).

## Run locally

1. Open this folder in RStudio and set the working directory to the project root (**Session → Set Working Directory → To Source File Location** when `app.R` is open).

2. Install dependencies:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "DT", "bsicons", "rsconnect"))
```

3. Start the app:

```r
shiny::runApp("app.R")
```

## Deploy to Posit Connect Cloud

Same workflow as [DSCI-532-Foodlytics-r](https://github.com/rabin0208/DSCI-532-Foodlytics-r):

1. Ensure `manifest.json` sits next to `app.R` (regenerate after adding packages or changing dependencies):

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

2. Commit and push `app.R`, `manifest.json`, and `data/international/` to GitHub.

3. In [Posit Connect Cloud](https://connect.posit.cloud/), connect the repo and deploy. Connect uses `manifest.json` to restore R packages.

**Note:** The deployed app reads CSVs from `data/international/` in the repo. Keep those files in version control (or adjust paths if you later load from BigQuery instead).

## Data

| File | Description |
|------|-------------|
| `results.csv` | International matches (date, teams, scores, tournament, venue) |
| `goalscorers.csv` | Goal-level events linked to matches |
| `shootouts.csv` | Penalty shootout outcomes |
| `former_names.csv` | Historical country name mappings |

Ingestion to GCS/BigQuery: `upload_international_results.R`.

## Dashboard (starter views)

- **Filters:** year range, tournament, team, neutral venue
- **Overview:** matches and goals per year
- **Teams:** most active teams in the selection
- **Goalscorers:** top scorers for filtered matches
- **Results:** searchable match table

You can extend tabs later (e.g. World Cup-only views, shootouts, home vs away win rates).
