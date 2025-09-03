#===============================================================================
# Libraries and helper functions
#===============================================================================

library(tidyverse)
library(nflreadr)
source("Scripts/constants.R")

# Build enriched match info for a season
build_match_information <- function(season, teams) {
  nflreadr::load_schedules(seasons = season) |>
    left_join(teams[, c("team_abbr", "team_name")], by = c("home_team" = "team_abbr")) |>
    rename(home_team_full = team_name) |>
    left_join(teams[, c("team_abbr", "team_name")], by = c("away_team" = "team_abbr")) |>
    rename(away_team_full = team_name) |>
    mutate(match = paste(home_team_full, "v", away_team_full)) |>
    select(
      match,
      home_team = home_team_full,
      home_abbr = home_team,
      away_team = away_team_full,
      away_abbr = away_team,
      season,
      game_type,
      week,
      date = gameday,
      time = gametime,
      venue = stadium,
      roof_status = roof,
      referee,
      home_score,
      home_qb = home_qb_name,
      home_coach,
      home_days_rest = home_rest,
      away_score,
      away_qb = away_qb_name,
      away_coach,
      away_days_rest = away_rest,
      total,
      overtime
    )
}

# Load offense player stats for a season
load_offense_stats <- function(season) {
  nflreadr::load_player_stats(seasons = season, stat_type = "offense") |>
    select(
      player_name = player_display_name,
      player_team = recent_team,
      position,
      season,
      week,
      season_type,
      completions:fantasy_points_ppr
    )
}

# Build combined (home+away) offense stats joined to matches for a season
build_offense_by_game <- function(season, teams) {
  match_info <- build_match_information(season, teams)
  offense <- load_offense_stats(season)

  home <- match_info |>
    inner_join(offense, by = c("home_abbr" = "player_team", "season", "week")) |>
    mutate(player_team = home_team, opposition_team = away_team) |>
    relocate(player_team, opposition_team, .after = player_name)

  away <- match_info |>
    inner_join(offense, by = c("away_abbr" = "player_team", "season", "week")) |>
    mutate(player_team = away_team, opposition_team = home_team) |>
    relocate(player_team, opposition_team, .after = player_name)

  bind_rows(home, away) |>
    arrange(date, time, match, player_team, player_name)
}

#===============================================================================
# Fetch and write for target seasons
#===============================================================================

teams <- load_teams()

# Include historical seasons and current season
historical_seasons <- c(2023, 2024)
seasons <- sort(unique(c(historical_seasons, CURRENT_SEASON)))

for (s in seasons) {
  offense_all <- build_offense_by_game(s, teams)
  write_rds(offense_all, paste0("Data/player_stats_", s, "_offense_all.rds"))
}
