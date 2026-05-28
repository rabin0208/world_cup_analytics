create_ui <- function(wc_upcoming, wc_fixture_choices) {
  page_fluid(
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
        selectInput(
          "h2h_scope",
          "Competition scope",
          choices = c(
            "All competitions" = "all",
            "FIFA World Cup only" = "wc"
          ),
          selected = "all"
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
        card_header("2026 FIFA World Cup - fixture matchup"),
        uiOutput("wc_fixture_summary"),
        tags$hr(),
        card_header("Head-to-head (wins/draws)"),
        plotOutput("wc_h2h_counts_plot", height = "320px")
      ),
      layout_columns(
        card(
          card_header("Head-to-head results"),
          dataTableOutput("wc_h2h_all"),
          full_screen = TRUE
        ),
        col_widths = 12,
        fill = FALSE
      ),
      card(
        card_header("Last 10 games - selected home team"),
        dataTableOutput("wc_home_last10"),
        full_screen = TRUE
      ),
      card(
        card_header("Last 10 games - selected away team"),
        dataTableOutput("wc_away_last10"),
        full_screen = TRUE
      )
    )
  )
}
