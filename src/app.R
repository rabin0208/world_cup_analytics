library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(bigrquery)

source_local <- function(rel_path) {
  if (file.exists(rel_path)) {
    source(rel_path)
    return(invisible(NULL))
  }
  up_one <- file.path("..", rel_path)
  if (file.exists(up_one)) {
    source(up_one)
    return(invisible(NULL))
  }
  stop("Could not source required file: ", rel_path)
}

source_local("src/R/config.R")
source_local("src/R/analytics.R")
source_local("src/R/ui.R")
source_local("src/R/server.R")

project_root <- get_project_root()
env_path <- file.path(project_root, ".env")
if (file.exists(env_path)) {
  readRenviron(env_path)
}

data_dir <- file.path(project_root, "data", "international")
results <- load_results(data_dir, project_root)
results <- prepare_results(results)

wc_results <- results %>% filter(is_wc_tournament(tournament))
wc_played <- wc_results %>% filter(is_played(.))
wc_upcoming <- wc_results %>%
  filter(!is_played(.)) %>%
  arrange(date, home_team)
played_results <- results %>% filter(is_played(.))

wc_fixture_choices <- build_fixture_choices(wc_upcoming)

ui <- create_ui(wc_upcoming, wc_fixture_choices)
server <- create_server(wc_played, wc_upcoming, played_results)

shinyApp(ui, server)
