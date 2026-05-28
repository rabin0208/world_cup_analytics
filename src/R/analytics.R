is_wc_tournament <- function(x) {
  grepl("FIFA World Cup", x, fixed = TRUE) &
    !grepl("qualification", x, ignore.case = TRUE)
}

is_played <- function(home_score, away_score) {
  !is.na(home_score) & !is.na(away_score)
}

h2h_matches <- function(team_a, team_b, data) {
  data |>
    filter(
      (home_team == team_a & away_team == team_b) |
        (home_team == team_b & away_team == team_a)
    ) |>
    arrange(desc(date))
}

h2h_summary <- function(team_a, team_b, data) {
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
    "%s -- %s vs %s (%s)",
    format(row$date, "%Y-%m-%d"),
    row$home_team,
    row$away_team,
    row$city
  )
}

build_fixture_choices <- function(wc_upcoming) {
  if (nrow(wc_upcoming) == 0) {
    return(character())
  }

  stats::setNames(
    seq_len(nrow(wc_upcoming)),
    vapply(seq_len(nrow(wc_upcoming)), function(i) fixture_label(wc_upcoming[i, ]), character(1))
  )
}

format_h2h_table <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame(Message = "No prior meetings."))
  }
  d |>
    mutate(
      Result = sprintf("%d - %d", home_score, away_score)
    ) |>
    select(
      Date = date,
      Home = home_team,
      Away = away_team,
      Result,
      Tournament = tournament,
      City = city
    )
}

team_last_matches <- function(team_name, played_results, scope) {
  d <- played_results |>
    filter(home_team == team_name | away_team == team_name)
  if (identical(scope, "wc")) {
    d <- d |> filter(is_wc_tournament(tournament))
  }

  d |>
    arrange(desc(date)) |>
    slice_head(n = 10) |>
    mutate(
      Result = sprintf("%d - %d", home_score, away_score),
      Venue = ifelse(home_team == team_name, "Home", "Away"),
      Team = team_name,
      Opponent = ifelse(home_team == team_name, away_team, home_team)
    ) |>
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
