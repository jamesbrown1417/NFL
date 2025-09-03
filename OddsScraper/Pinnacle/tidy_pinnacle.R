# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)
source("Scripts/constants.R")

# Get Fix Team Names Function
source("Scripts/fix_team_names.R")

# Get Fix Player Names Function
source("Scripts/fix_player_names.R")

# Get squads
player_teams <-
  load_rosters(seasons = CURRENT_SEASON) |> 
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
  mutate(player_name = str_remove(selection, " (\\(Anytime TD\\)|Anytime TD)$")) |>
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
  mutate(player_name = str_remove(selection, " (\\(Anytime TD\\)|Anytime TD)$")) |>
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

#===============================================================================
# Passings TDs
#===============================================================================

# Get all passing TD Markets
passing_td_markets <-
all_pinnacle_data |> 
  filter(str_detect(selection, "Touchdown Passes"))

# Passing TDs - over
passing_td_over <-
  passing_td_markets |> 
  mutate(player_name = str_remove(selection, " Total Touchdown Passes")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Passing Touchdowns") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_qb
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

# Passing TDs - under
passing_td_under <-
  passing_td_markets |> 
  mutate(player_name = str_remove(selection, " Total Touchdown Passes")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Passing Touchdowns") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_qb
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
passing_td_data <-
  inner_join(passing_td_over, passing_td_under) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
passing_td_data |> 
  write_csv("Data/scraped_odds/pinnacle_passing_td.csv")

#===============================================================================
# Pass Attempts
#===============================================================================

# Get all passing attempts Markets
passing_attempts_markets <-
all_pinnacle_data |> 
  filter(str_detect(selection, "Pass Attempts"))

# Passing Attempts - over
passing_attempts_over <-
  passing_attempts_markets |> 
  mutate(player_name = str_remove(selection, " Total Pass Attempts")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Passing Attempts") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_qb
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

# Passing Attempts - under
passing_attempts_under <-
  passing_attempts_markets |> 
  mutate(player_name = str_remove(selection, " Total Pass Attempts")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Passing Attempts") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_qb
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
passing_attempts_data <-
  inner_join(passing_attempts_over, passing_attempts_under) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
passing_attempts_data |> 
  write_csv("Data/scraped_odds/pinnacle_passing_attempts.csv")

#===============================================================================
# Receptions
#===============================================================================

# Get all receptions Markets
receptions_markets <-
all_pinnacle_data |> 
  filter(str_detect(selection, "Receptions"))

# Receptions - over
receptions_over <-
  receptions_markets |> 
  mutate(player_name = str_remove(selection, " Total Receptions")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Receptions") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_rb,
      player_teams_wr,
      player_teams_te
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

# Receptions - under
receptions_under <-
  receptions_markets |> 
  mutate(player_name = str_remove(selection, " Total Receptions")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Receptions") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_rb,
      player_teams_wr,
      player_teams_te
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
receptions_data <-
  inner_join(receptions_over, receptions_under) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
receptions_data |> 
  write_csv("Data/scraped_odds/pinnacle_receptions.csv")

#===============================================================================
# Passing Yards
#===============================================================================

# Get all passing yards Markets
passing_yards_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Passing Yards"))

# Passing Yards - over
passing_yards_over <-
  passing_yards_markets |> 
  mutate(player_name = str_remove(selection, " Total Passing Yards")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Passing Yards") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_qb
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

# Passing Yards - under
passing_yards_under <-
  passing_yards_markets |> 
  mutate(player_name = str_remove(selection, " Total Passing Yards")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Passing Yards") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_qb
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
passing_yards_data <-
  inner_join(passing_yards_over, passing_yards_under) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
passing_yards_data |> 
  write_csv("Data/scraped_odds/pinnacle_passing_yards.csv")

#===============================================================================
# Receiving Yards
#===============================================================================

# Get all receiving yards Markets
receiving_yards_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Receiving Yards"))

# Receiving Yards - over
receiving_yards_over <-
  receiving_yards_markets |> 
  mutate(player_name = str_remove(selection, " Total Receiving Yards")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Receiving Yards") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_rb,
      player_teams_qb,
      player_teams_wr,
      player_teams_te
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

# Receiving Yards - under
receiving_yards_under <-
  receiving_yards_markets |> 
  mutate(player_name = str_remove(selection, " Total Receiving Yards")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Receiving Yards") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_rb,
      player_teams_qb,
      player_teams_wr,
      player_teams_te
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
receiving_yards_data <-
  inner_join(receiving_yards_over, receiving_yards_under) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
receiving_yards_data |> 
  write_csv("Data/scraped_odds/pinnacle_receiving_yards.csv")

#===============================================================================
# Rushing Yards
#===============================================================================

# Get all rushing yards Markets
rushing_yards_markets <-
  all_pinnacle_data |> 
  filter(str_detect(selection, "Rushing Yards"))

# Rushing Yards - over
rushing_yards_over <-
  rushing_yards_markets |> 
  mutate(player_name = str_remove(selection, " Total Rushing Yards")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Over")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Rushing Yards") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_rb,
      player_teams_qb,
      player_teams_wr,
      player_teams_te
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

# Rushing Yards - under
rushing_yards_under <-
  rushing_yards_markets |> 
  mutate(player_name = str_remove(selection, " Total Rushing Yards")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  filter(str_detect(name, "Under")) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team),
         player_name = fix_player_names(player_name)) |>
  mutate(match = glue("{home_team} v {away_team}")) |> 
  mutate(agency = "Pinnacle") |>
  mutate(market = "Rushing Yards") |>
  mutate(line = handicap) |> 
  left_join(
    bind_rows(
      player_teams_rb,
      player_teams_qb,
      player_teams_wr,
      player_teams_te
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
rushing_yards_data <-
  inner_join(rushing_yards_over, rushing_yards_under) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
rushing_yards_data |> 
  write_csv("Data/scraped_odds/pinnacle_rushing_yards.csv")
