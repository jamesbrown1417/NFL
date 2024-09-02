#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(nflreadr)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Get all stats
#===============================================================================

# Get match information
match_information_2023 <-
  nflreadr::load_schedules(seasons = 2023) |>
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

# Get player stats--------------------------------------------------------------

# All stats
player_stats_2023_offense <-
  nflreadr::load_player_stats(seasons = 2023, stat_type = "offense") |> 
  select(player_name = player_display_name,
         player_team = recent_team,
         opponent_team,
         season,
         week,
         season_type,
         completions:fantasy_points_ppr)

player_stats_2023_defense <-
  nflreadr::load_player_stats(seasons = 2023, stat_type = "defense") |> 
  select(player_name = player_display_name,
         player_team = recent_team,
         opponent_team,
         season,
         week,
         season_type,
         tackles:interceptions)

# Get home games
home_player_stats_offense <-
match_information_2023 |> 
  inner_join(player_stats_2023, by = c("home_abbr" = "player_team", "season", "week"))
