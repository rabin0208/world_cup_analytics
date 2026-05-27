library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)

data_dir <- "data/international"
if (!dir.exists(data_dir)) {
  stop("Expected data at data/international/ (run from project root).")
}

results <- read.csv(
  file.path(data_dir, "results.csv"),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)
goalscorers <- read.csv(
  file.path(data_dir, "goalscorers.csv"),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

results$date <- as.Date(results$date)
results$year <- as.integer(format(results$date, "%Y"))
results$total_goals <- results$home_score + results$away_score
results$neutral <- as.logical(results$neutral)

goalscorers$date <- as.Date(goalscorers$date)

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

teams <- sort(unique(c(results$home_team, results$away_team)))
tournaments <- sort(unique(results$tournament[!is.na(results$tournament)]))
year_min <- min(results$year, na.rm = TRUE)
year_max <- max(results$year, na.rm = TRUE)

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
        value = format(nrow(wc_played), big.mark = ","),
        showcase = bsicons::bs_icon("clock-history")
      ),
      fill = FALSE
    ),
    card(
      card_header("2026 FIFA World Cup — fixture matchup"),
      uiOutput("wc_fixture_summary"),
      tags$hr(),
      card_header("Head-to-head (wins/draws, all competitions)"),
      uiOutput("wc_h2h_counts")
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
      card_header("All 2026 group-stage fixtures"),
      dataTableOutput("wc_upcoming_tbl"),
      full_screen = TRUE
    )
  )
)

server <- function(input, output, session) {
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

  output$wc_h2h_counts <- renderUI({
    fx <- selected_wc_fixture()
    if (is.null(fx)) {
      return(p(class = "text-muted", "Select a fixture."))
    }

    s <- h2h_summary(fx$home_team, fx$away_team, played_results)
    if (s$matches == 0) {
      return(p(class = "text-muted", "No previous meetings between these teams in your dataset."))
    }

    tagList(
      tags$p(class = "text-muted mb-2", sprintf("Meetings: %d (all competitions)", s$matches)),
      layout_columns(
        value_box(
          title = fx$home_team,
          value = s$team_a_wins,
          showcase = bsicons::bs_icon("arrow-up")
        ),
        value_box(
          title = "Draws",
          value = s$draws,
          showcase = bsicons::bs_icon("dash")
        ),
        value_box(
          title = fx$away_team,
          value = s$team_b_wins,
          showcase = bsicons::bs_icon("arrow-down")
        ),
        fill = FALSE
      )
    )
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
    format_h2h_table(h2h_matches(fx$home_team, fx$away_team, played_results))
  }, options = list(pageLength = 8, order = list(list(0, "desc"))))

  output$wc_upcoming_tbl <- renderDataTable({
    wc_upcoming %>%
      mutate(
        Date = format(date, "%Y-%m-%d"),
        Fixture = paste(home_team, "vs", away_team),
        Venue = paste(city, country, sep = ", ")
      ) %>%
      select(Date, Fixture, Venue, Neutral = neutral)
  }, options = list(pageLength = 15, order = list(list(0, "asc"))))
}

shinyApp(ui, server)
