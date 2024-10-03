# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)

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

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

# Read in data
all_pinnacle_raw_files <- list.files("OddsScraper/Pinnacle", "\\.csv")