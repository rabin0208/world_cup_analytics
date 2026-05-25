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
}

shinyApp(ui, server)
