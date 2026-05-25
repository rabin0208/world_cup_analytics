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

ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  title = "International Football Analytics",
  layout_sidebar(
    sidebar = sidebar(
      tags$label("Year range", style = "font-weight: bold;"),
      sliderInput(
        "year_range",
        label = "",
        min = year_min,
        max = year_max,
        value = c(max(year_min, year_max - 30L), year_max),
        sep = ""
      ),
      tags$label("Tournament", style = "font-weight: bold;"),
      selectizeInput(
        "tournament",
        label = "",
        choices = tournaments,
        selected = c("FIFA World Cup", "Friendly"),
        multiple = TRUE,
        options = list(placeholder = "All tournaments")
      ),
      tags$label("Team (home or away)", style = "font-weight: bold;"),
      selectizeInput(
        "team",
        label = "",
        choices = c("All teams" = "", teams),
        selected = ""
      ),
      checkboxInput("neutral_only", "Neutral venue only", FALSE),
      actionButton("reset_filters", "Reset filters"),
      open = "desktop"
    ),
    layout_columns(
      value_box(
        title = "Matches",
        value = textOutput("n_matches"),
        showcase = bsicons::bs_icon("trophy")
      ),
      value_box(
        title = "Total goals",
        value = textOutput("total_goals"),
        showcase = bsicons::bs_icon("graph-up")
      ),
      value_box(
        title = "Avg goals / match",
        value = textOutput("avg_goals"),
        showcase = bsicons::bs_icon("calculator")
      ),
      fill = FALSE
    ),
    navset_card_tab(
      nav_panel(
        "Overview",
        layout_columns(
          card(
            card_header("Matches per year"),
            plotOutput("plot_matches_year", height = "320px"),
            full_screen = TRUE
          ),
          card(
            card_header("Goals per year"),
            plotOutput("plot_goals_year", height = "320px"),
            full_screen = TRUE
          ),
          col_widths = c(6, 6)
        )
      ),
      nav_panel(
        "Teams",
        card(
          card_header("Most active teams (filtered)"),
          plotOutput("plot_top_teams", height = "400px"),
          full_screen = TRUE
        )
      ),
      nav_panel(
        "Goalscorers",
        card(
          card_header("Top scorers (filtered matches)"),
          plotOutput("plot_top_scorers", height = "400px"),
          full_screen = TRUE
        )
      ),
      nav_panel(
        "Results",
        card(
          card_header("Match results"),
          dataTableOutput("tbl_results"),
          full_screen = TRUE
        )
      ),
      nav_panel(
        "World Cup",
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
        layout_columns(
          card(
            card_header("2026 FIFA World Cup — upcoming fixtures"),
            p(
              class = "text-muted small mb-2",
              "Select a fixture to preview head-to-head history before kickoff ",
              "(all competitions and World Cup-only)."
            ),
            selectInput(
              "wc_fixture",
              "Fixture",
              choices = wc_fixture_choices,
              selected = if (length(wc_fixture_choices)) wc_fixture_choices[[1]] else character()
            ),
            uiOutput("wc_fixture_summary"),
            col_widths = 12
          ),
          col_widths = 12
        ),
        layout_columns(
          card(
            card_header("Head-to-head summary"),
            uiOutput("wc_h2h_summary"),
            full_screen = TRUE
          ),
          card(
            card_header("Head-to-head results (all competitions)"),
            dataTableOutput("wc_h2h_all"),
            full_screen = TRUE
          ),
          col_widths = c(4, 8)
        ),
        card(
          card_header("World Cup head-to-head only"),
          dataTableOutput("wc_h2h_wc"),
          full_screen = TRUE
        ),
        card(
          card_header("All 2026 group-stage fixtures"),
          dataTableOutput("wc_upcoming_tbl"),
          full_screen = TRUE
        )
      )
    )
  )
)

filter_results <- function(data, year_range, tournament, team, neutral_only) {
  out <- data %>%
    filter(year >= year_range[1], year <= year_range[2])

  if (length(tournament) > 0) {
    out <- out %>% filter(tournament %in% tournament)
  }
  if (nzchar(team)) {
    out <- out %>% filter(home_team == team | away_team == team)
  }
  if (isTRUE(neutral_only)) {
    out <- out %>% filter(neutral %in% TRUE)
  }
  out
}

server <- function(input, output, session) {
  filtered_results <- reactive({
    filter_results(
      results,
      input$year_range,
      input$tournament,
      input$team,
      input$neutral_only
    )
  })

  filtered_goalscorers <- reactive({
    d <- filtered_results()
    if (nrow(d) == 0) {
      return(goalscorers[0, , drop = FALSE])
    }
    keys <- paste(d$date, d$home_team, d$away_team, sep = "|")
    goalscorers %>%
      mutate(.key = paste(date, home_team, away_team, sep = "|")) %>%
      filter(.key %in% keys) %>%
      select(-.key)
  })

  observeEvent(input$reset_filters, {
    updateSliderInput(
      session,
      "year_range",
      value = c(max(year_min, year_max - 30L), year_max)
    )
    updateSelectizeInput(
      session,
      "tournament",
      selected = c("FIFA World Cup", "Friendly")
    )
    updateSelectizeInput(session, "team", selected = "")
    updateCheckboxInput(session, "neutral_only", value = FALSE)
  })

  output$n_matches <- renderText({
    format(nrow(filtered_results()), big.mark = ",")
  })

  output$total_goals <- renderText({
    d <- filtered_results()
    if (nrow(d) == 0) return("—")
    format(sum(d$total_goals, na.rm = TRUE), big.mark = ",")
  })

  output$avg_goals <- renderText({
    d <- filtered_results()
    if (nrow(d) == 0) return("—")
    sprintf("%.2f", mean(d$total_goals, na.rm = TRUE))
  })

  empty_plot <- function(msg = "No matches match the selected filters.") {
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
    text(0.5, 0.5, msg, cex = 1.1)
    invisible(NULL)
  }

  output$plot_matches_year <- renderPlot({
    d <- filtered_results()
    if (nrow(d) == 0) return(empty_plot())
    agg <- d %>% count(year, name = "matches")
    ggplot(agg, aes(x = year, y = matches)) +
      geom_col(fill = "#2171b5") +
      labs(x = "Year", y = "Matches") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"))
  })

  output$plot_goals_year <- renderPlot({
    d <- filtered_results()
    if (nrow(d) == 0) return(empty_plot())
    agg <- d %>%
      group_by(year) %>%
      summarise(goals = sum(total_goals, na.rm = TRUE), .groups = "drop")
    ggplot(agg, aes(x = year, y = goals)) +
      geom_line(color = "#238b45", linewidth = 1) +
      geom_point(color = "#238b45", size = 1.5) +
      labs(x = "Year", y = "Goals") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"))
  })

  output$plot_top_teams <- renderPlot({
    d <- filtered_results()
    if (nrow(d) == 0) return(empty_plot())
    agg <- bind_rows(
      d %>% transmute(team = home_team),
      d %>% transmute(team = away_team)
    ) %>%
      count(team, name = "matches") %>%
      slice_max(matches, n = 15, with_ties = FALSE) %>%
      mutate(team = factor(team, levels = team[order(matches)]))
    ggplot(agg, aes(x = matches, y = team, fill = matches)) +
      geom_col() +
      scale_fill_gradient(low = "#c6dbef", high = "#084594", guide = "none") +
      labs(x = "Matches played", y = NULL) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"))
  })

  output$plot_top_scorers <- renderPlot({
    g <- filtered_goalscorers() %>%
      filter(!own_goal %in% TRUE)
    if (nrow(g) == 0) return(empty_plot("No goalscorer rows for the current filters."))
    agg <- g %>%
      count(scorer, team, name = "goals") %>%
      slice_max(goals, n = 15, with_ties = FALSE) %>%
      mutate(label = paste0(scorer, " (", team, ")")) %>%
      mutate(label = factor(label, levels = label[order(goals)]))
    ggplot(agg, aes(x = goals, y = label, fill = goals)) +
      geom_col() +
      scale_fill_gradient(low = "#c7e9c0", high = "#006d2c", guide = "none") +
      labs(x = "Goals", y = NULL) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white"))
  })

  output$tbl_results <- renderDataTable({
    filtered_results() %>%
      arrange(desc(date)) %>%
      select(
        Date = date,
        Home = home_team,
        `H score` = home_score,
        Away = away_team,
        `A score` = away_score,
        Tournament = tournament,
        City = city,
        Country = country,
        Neutral = neutral
      )
  }, options = list(pageLength = 10, order = list(list(0, "desc"))))

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

  wc_h2h_reactive <- reactive({
    fx <- selected_wc_fixture()
    if (is.null(fx)) {
      return(list(all = NULL, wc = NULL, summary_all = NULL, summary_wc = NULL))
    }
    list(
      all = h2h_matches(fx$home_team, fx$away_team, played_results),
      wc = h2h_matches(fx$home_team, fx$away_team, wc_played),
      summary_all = h2h_summary(fx$home_team, fx$away_team, played_results),
      summary_wc = h2h_summary(fx$home_team, fx$away_team, wc_played)
    )
  })

  output$wc_h2h_summary <- renderUI({
    fx <- selected_wc_fixture()
    h2h <- wc_h2h_reactive()
    if (is.null(fx)) {
      return(p(class = "text-muted", "Select a fixture above."))
    }
    s_all <- h2h$summary_all
    s_wc <- h2h$summary_wc
    if (s_all$matches == 0) {
      return(p(
        class = "text-muted",
        sprintf(
          "No previous meetings between %s and %s in this dataset.",
          fx$home_team,
          fx$away_team
        )
      ))
    }
    tagList(
      tags$p(
        tags$strong("All competitions"),
        sprintf(
          " (%d meetings): %s %dW · %dD · %dL %s — goals %d–%d",
          s_all$matches,
          fx$home_team,
          s_all$team_a_wins,
          s_all$draws,
          s_all$team_b_wins,
          fx$away_team,
          s_all$team_a_goals,
          s_all$team_b_goals
        )
      ),
      if (s_wc$matches > 0) {
        tags$p(
          tags$strong("World Cup only"),
          sprintf(
            " (%d meetings): %s %dW · %dD · %dL %s — goals %d–%d",
            s_wc$matches,
            fx$home_team,
            s_wc$team_a_wins,
            s_wc$draws,
            s_wc$team_b_wins,
            fx$away_team,
            s_wc$team_a_goals,
            s_wc$team_b_goals
          )
        )
      } else {
        tags$p(
          class = "text-muted",
          "These teams have not met at a FIFA World Cup finals tournament before."
        )
      }
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
    format_h2h_table(wc_h2h_reactive()$all)
  }, options = list(pageLength = 8, order = list(list(0, "desc"))))

  output$wc_h2h_wc <- renderDataTable({
    format_h2h_table(wc_h2h_reactive()$wc)
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
