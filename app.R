library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(bigrquery)

data_dir <- "data/international"
if (!dir.exists(data_dir)) {
  stop("Expected data at data/international/ (run from project root).")
}

if (file.exists(".env")) {
  readRenviron(".env")
}

env_is_true <- function(x) {
  v <- toupper(trimws(x))
  nzchar(v) && v %in% c("1", "TRUE", "YES", "Y")
}

load_results <- function(data_dir) {
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
    normalizePath(file.path(getwd(), cred_raw), mustWork = TRUE)
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

results <- load_results(data_dir)

results$date <- as.Date(results$date)
results$year <- as.integer(format(results$date, "%Y"))
results$total_goals <- results$home_score + results$away_score
results$neutral <- as.logical(results$neutral)

is_wc_tournament <- function(x) {
  grepl("FIFA World Cup", x, fixed = TRUE) &
    !grepl("qualification", x, ignore.case = TRUE)
}

is_played <- function(d) {
  !is.na(d$home_score) & !is.na(d$away_score)
}

wc_results <- results %>% filter(is_wc_tournament(tournament))
wc_played <- wc_results %>% filter(is_played(.))
wc_upcoming <- wc_results %>%
  filter(!is_played(.)) %>%
  arrange(date, home_team)

played_results <- results %>% filter(is_played(.))

h2h_matches <- function(team_a, team_b, data = played_results) {
  data %>%
    filter(
      (home_team == team_a & away_team == team_b) |
        (home_team == team_b & away_team == team_a)
    ) %>%
    arrange(desc(date))
}

h2h_summary <- function(team_a, team_b, data = played_results) {
  m <- h2h_matches(team_a, team_b, data)
  if (nrow(m) == 0) {
    return(list(
      matches = 0L,
      team_a_wins = 0L,
      draws = 0L,
      team_b_wins = 0L,
      team_a_goals = 0L,
      team_b_goals = 0L
    ))
  }
  pts <- lapply(seq_len(nrow(m)), function(i) {
    row <- m[i, ]
    if (row$home_team == team_a) {
      ga <- row$home_score
      gb <- row$away_score
    } else {
      ga <- row$away_score
      gb <- row$home_score
    }
    if (ga > gb) {
      c(wa = 1L, dr = 0L, wb = 0L, ga = ga, gb = gb)
    } else if (ga < gb) {
      c(wa = 0L, dr = 0L, wb = 1L, ga = ga, gb = gb)
    } else {
      c(wa = 0L, dr = 1L, wb = 0L, ga = ga, gb = gb)
    }
  })
  mat <- do.call(rbind, pts)
  list(
    matches = nrow(m),
    team_a_wins = sum(mat[, "wa"]),
    draws = sum(mat[, "dr"]),
    team_b_wins = sum(mat[, "wb"]),
    team_a_goals = sum(mat[, "ga"]),
    team_b_goals = sum(mat[, "gb"])
  )
}

fixture_label <- function(row) {
  sprintf(
    "%s — %s vs %s (%s)",
    format(row$date, "%Y-%m-%d"),
    row$home_team,
    row$away_team,
    row$city
  )
}

wc_fixture_choices <- if (nrow(wc_upcoming) > 0) {
  stats::setNames(
    seq_len(nrow(wc_upcoming)),
    vapply(seq_len(nrow(wc_upcoming)), function(i) fixture_label(wc_upcoming[i, ]), character(1))
  )
} else {
  character()
}

h2h_competitions <- sort(unique(played_results$tournament[!is.na(played_results$tournament)]))
year_min <- min(played_results$year, na.rm = TRUE)
year_max <- max(played_results$year, na.rm = TRUE)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  title = "World Cup Analytics",
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      tags$h4("Fixture Filter"),
      p(
        class = "text-muted small",
        "Select a 2026 World Cup fixture to see head-to-head wins and draws."
      ),
      selectInput(
        "wc_fixture",
        "Fixture",
        choices = wc_fixture_choices,
        selected = if (length(wc_fixture_choices)) wc_fixture_choices[[1]] else character()
      ),
      sliderInput(
        "h2h_year_range",
        "Head-to-head year range",
        min = year_min,
        max = year_max,
        value = c(year_min, year_max),
        sep = "",
        ticks = FALSE
      ),
      selectizeInput(
        "h2h_competition",
        "Competition",
        choices = h2h_competitions,
        selected = character(),
        multiple = TRUE,
        options = list(
          placeholder = "All competitions"
        )
      ),
      open = "desktop"
    ),
    layout_columns(
      value_box(
        title = "2026 fixtures (unplayed)",
        value = nrow(wc_upcoming),
        showcase = bsicons::bs_icon("calendar-event")
      ),
      value_box(
        title = "World Cup matches (played)",
      value = textOutput("wc_played_count"),
        showcase = bsicons::bs_icon("clock-history")
      ),
      fill = FALSE
    ),
    card(
      card_header("2026 FIFA World Cup — fixture matchup"),
      uiOutput("wc_fixture_summary"),
      tags$hr(),
      card_header("Head-to-head (wins/draws, all competitions)"),
      plotOutput("wc_h2h_counts_plot", height = "320px")
    ),
    layout_columns(
      card(
        card_header("Head-to-head results (all competitions)"),
        dataTableOutput("wc_h2h_all"),
        full_screen = TRUE
      ),
      col_widths = 12,
      fill = FALSE
    ),
    card(
      card_header("Last 10 games — selected home team"),
      dataTableOutput("wc_home_last10"),
      full_screen = TRUE
    ),
    card(
      card_header("Last 10 games — selected away team"),
      dataTableOutput("wc_away_last10"),
      full_screen = TRUE
    )
  )
)

server <- function(input, output, session) {
  output$wc_played_count <- renderText({
    format(
      nrow(
        wc_played %>%
          filter(
            year >= input$h2h_year_range[1],
            year <= input$h2h_year_range[2]
          )
      ),
      big.mark = ","
    )
  })

  selected_wc_fixture <- reactive({
    if (is.null(input$wc_fixture) || !nzchar(input$wc_fixture)) {
      return(NULL)
    }
    idx <- as.integer(input$wc_fixture)
    if (is.na(idx) || idx < 1L || idx > nrow(wc_upcoming)) {
      return(NULL)
    }
    wc_upcoming[idx, , drop = FALSE]
  })

  output$wc_fixture_summary <- renderUI({
    fx <- selected_wc_fixture()
    if (is.null(fx)) {
      return(p(class = "text-muted", "No upcoming World Cup fixtures in the dataset."))
    }
    tagList(
      tags$h4(
        sprintf("%s vs %s", fx$home_team, fx$away_team),
        class = "mb-1"
      ),
      tags$p(
        class = "text-muted mb-0",
        sprintf(
          "%s · %s, %s · %s venue",
          format(fx$date, "%A, %d %B %Y"),
          fx$city,
          fx$country,
          if (isTRUE(fx$neutral)) "neutral" else "non-neutral"
        )
      )
    )
  })

  h2h_filtered_matches <- reactive({
    fx <- selected_wc_fixture()
    if (is.null(fx)) {
      return(played_results[0, , drop = FALSE])
    }

    d <- h2h_matches(fx$home_team, fx$away_team, played_results) %>%
      filter(
        year >= input$h2h_year_range[1],
        year <= input$h2h_year_range[2]
      )

    if (length(input$h2h_competition) > 0) {
      d <- d %>% filter(tournament %in% input$h2h_competition)
    }

    d
  })

  output$wc_h2h_counts_plot <- renderPlot({
    fx <- selected_wc_fixture()
    if (is.null(fx)) {
      plot.new()
      text(0.5, 0.5, "Select a fixture.", cex = 1.1)
      return(invisible(NULL))
    }

    s <- h2h_summary(fx$home_team, fx$away_team, h2h_filtered_matches())
    if (s$matches == 0) {
      plot.new()
      text(0.5, 0.5, "No previous meetings between these teams in your dataset.", cex = 1.0)
      return(invisible(NULL))
    }

    counts <- data.frame(
      outcome = factor(
        c(fx$home_team, "Draws", fx$away_team),
        levels = c(fx$home_team, "Draws", fx$away_team)
      ),
      n = c(s$team_a_wins, s$draws, s$team_b_wins)
    )

    ggplot(counts, aes(x = outcome, y = n, fill = outcome)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = n), vjust = -0.4, size = 6) +
      scale_fill_manual(values = c("#2ca25f", "#636363", "#de2d26")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        x = NULL,
        y = "Head-to-head matches",
        subtitle = sprintf(
          "Meetings: %d | Years: %d-%d%s",
          s$matches,
          input$h2h_year_range[1],
          input$h2h_year_range[2],
          if (length(input$h2h_competition) > 0) " | Competition filtered" else ""
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(panel.background = element_rect(fill = "white"))
  })

  format_h2h_table <- function(d) {
    if (is.null(d) || nrow(d) == 0) {
      return(data.frame(Message = "No prior meetings."))
    }
    d %>%
      mutate(
        Result = sprintf("%d – %d", home_score, away_score)
      ) %>%
      select(
        Date = date,
        Home = home_team,
        Away = away_team,
        Result,
        Tournament = tournament,
        City = city
      )
  }

  output$wc_h2h_all <- renderDataTable({
    fx <- selected_wc_fixture()
    if (is.null(fx)) return(data.frame(Message = "Select a fixture above."))
    format_h2h_table(h2h_filtered_matches())
  }, options = list(pageLength = 8, order = list(list(0, "desc"))))

  team_last_matches <- function(team_name) {
    d <- played_results %>%
      filter(
        (home_team == team_name | away_team == team_name),
        year >= input$h2h_year_range[1],
        year <= input$h2h_year_range[2]
      )

    if (length(input$h2h_competition) > 0) {
      d <- d %>% filter(tournament %in% input$h2h_competition)
    }

    d %>%
      arrange(desc(date)) %>%
      slice_head(n = 10) %>%
      mutate(
        Result = sprintf("%d – %d", home_score, away_score),
        Venue = ifelse(home_team == team_name, "Home", "Away"),
        Team = team_name,
        Opponent = ifelse(home_team == team_name, away_team, home_team)
      ) %>%
      select(
        Date = date,
        Team,
        Opponent,
        Result,
        Venue,
        Tournament = tournament,
        City = city
      )
  }

  output$wc_home_last10 <- renderDataTable({
    fx <- selected_wc_fixture()
    if (is.null(fx)) return(data.frame(Message = "Select a fixture above."))
    d <- team_last_matches(fx$home_team)
    if (nrow(d) == 0) return(data.frame(Message = "No matches found for this team in the selected filters."))
    d
  }, options = list(pageLength = 10, order = list(list(0, "desc"))))

  output$wc_away_last10 <- renderDataTable({
    fx <- selected_wc_fixture()
    if (is.null(fx)) return(data.frame(Message = "Select a fixture above."))
    d <- team_last_matches(fx$away_team)
    if (nrow(d) == 0) return(data.frame(Message = "No matches found for this team in the selected filters."))
    d
  }, options = list(pageLength = 10, order = list(list(0, "desc"))))

}

shinyApp(ui, server)
