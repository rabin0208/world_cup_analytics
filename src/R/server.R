create_server <- function(wc_played, wc_upcoming, played_results) {
  function(input, output, session) {
    output$wc_played_count <- renderText({
      format(nrow(wc_played), big.mark = ",")
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
            "%s - %s, %s - %s venue",
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

      d <- h2h_matches(fx$home_team, fx$away_team, played_results)
      if (identical(input$h2h_scope, "wc")) {
        d <- d |> filter(is_wc_tournament(tournament))
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
            "Meetings: %d | Scope: %s",
            s$matches,
            if (identical(input$h2h_scope, "wc")) "FIFA World Cup only" else "All competitions"
          )
        ) +
        theme_minimal(base_size = 14) +
        theme(panel.background = element_rect(fill = "white"))
    })

    output$wc_h2h_all <- renderDataTable({
      fx <- selected_wc_fixture()
      if (is.null(fx)) return(data.frame(Message = "Select a fixture above."))
      format_h2h_table(h2h_filtered_matches())
    }, options = list(pageLength = 8, order = list(list(0, "desc"))))

    output$wc_home_last10 <- renderDataTable({
      fx <- selected_wc_fixture()
      if (is.null(fx)) return(data.frame(Message = "Select a fixture above."))
      d <- team_last_matches(fx$home_team, played_results, input$h2h_scope)
      if (nrow(d) == 0) return(data.frame(Message = "No matches found for this team in the selected filters."))
      d
    }, options = list(pageLength = 10, order = list(list(0, "desc"))))

    output$wc_away_last10 <- renderDataTable({
      fx <- selected_wc_fixture()
      if (is.null(fx)) return(data.frame(Message = "Select a fixture above."))
      d <- team_last_matches(fx$away_team, played_results, input$h2h_scope)
      if (nrow(d) == 0) return(data.frame(Message = "No matches found for this team in the selected filters."))
      d
    }, options = list(pageLength = 10, order = list(list(0, "desc"))))
  }
}
