# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

# Get squads
player_teams <-
  load_rosters(seasons = 2024) |> 
  select(player_name = full_name, position, player_team = team)

player_teams_qb <-
  player_teams |>
  filter(position == "QB") |> 
  mutate(player_team = fix_team_names(player_team)) |> 
  select(player_name, player_team) |> 
  mutate(player_team = ifelse(player_team == "LA", "Los Angeles Rams", player_team))

player_teams_rb <-
  player_teams |>
  filter(position == "RB") |> 
  mutate(player_team = fix_team_names(player_team)) |> 
  select(player_name, player_team) |> 
  mutate(player_team = ifelse(player_team == "LA", "Los Angeles Rams", player_team))

player_teams_wr <-
  player_teams |>
  filter(position == "WR") |> 
  mutate(player_team = fix_team_names(player_team)) |> 
  select(player_name, player_team) |> 
  mutate(player_team = ifelse(player_team == "LA", "Los Angeles Rams", player_team))

player_teams_te <-
  player_teams |>
  filter(position == "TE") |> 
  mutate(player_team = fix_team_names(player_team)) |> 
  select(player_name, player_team) |> 
  mutate(player_team = ifelse(player_team == "LA", "Los Angeles Rams", player_team))

player_teams_db <-
  player_teams |>
  filter(position == "DB") |> 
  mutate(player_team = fix_team_names(player_team)) |> 
  select(player_name, player_team) |> 
  mutate(player_team = ifelse(player_team == "LA", "Los Angeles Rams", player_team))

# Read in data
all_pinnacle_raw_files <- list.files("OddsScraper/Pinnacle", "\\.csv", full.names = TRUE)

all_pinnacle_data <-
  map(all_pinnacle_raw_files, read_csv) |> 
  bind_rows()

#===============================================================================
# Anytime TD Scorer
#===============================================================================

# Get all anytime TD Markets
anytime_td_markets <-
all_pinnacle_data |> 
  filter(str_detect(selection, "Anytime TD"))

# Anytime TD - yes
anytime_td_yes <-
  anytime_td_markets |> 
  mutate(player_name = str_remove(selection, " \\(Anytime TD\\)")) |>
  filter(str_detect(name, "Yes")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Touchdowns") |>
  mutate(line = 0.5) |> 
  left_join(
    bind_rows(
      player_teams_qb,
      player_teams_rb,
      player_teams_wr,
      player_teams_te,
      player_teams_db
    ),
    by = "player_name"
  ) |>
  select(
    match,
    player_name,
    player_team,
    market,
    line,
    over_price = price,
    agency) |> 
  filter(!is.na(over_price))

# Anytime TD - no
anytime_td_no <-
  anytime_td_markets |> 
  mutate(player_name = str_remove(selection, " \\(Anytime TD\\)")) |>
  filter(str_detect(name, "No")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Player Touchdowns") |>
  mutate(line = 0.5) |> 
  left_join(
    bind_rows(
      player_teams_qb,
      player_teams_rb,
      player_teams_wr,
      player_teams_te,
      player_teams_db
    ),
    by = "player_name"
  ) |>
  select(
    match,
    player_name,
    player_team,
    market,
    line,
    under_price = price,
    agency) |> 
  filter(!is.na(under_price))

# Combine
anytime_td_data <-
  inner_join(anytime_td_yes, anytime_td_no) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
anytime_td_data |> 
  write_csv("Data/scraped_odds/pinnacle_touchdowns.csv")
