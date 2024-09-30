# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)
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

#===============================================================================
# Get JSON for each match
#===============================================================================

# Read in df
df <- read_csv("OddsScraper/Neds/neds_match_urls.csv")

# Get match json files
json_match_files <- list.files("OddsScraper/Neds/", pattern = "^data_.*.json", full.names = TRUE)

event_json_list <- map(json_match_files, ~fromJSON(.x))

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
event_ids <- character()
entrants <- character()
entrant_ids <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
    match <- event_json_list[[i]] 
    
    for (entrant in match$entrants) {
        entrants <- c(entrants, entrant$name)
        market_id <- c(market_id, entrant$market_id)
        event_ids <- c(event_ids,  event_json_list[[i]]$events[[1]]$id)
        entrant_ids <- c(entrant_ids, entrant$id)
    } 
    
    
    # Loop through the markets
    for (market in match$markets) {
        market_lookup_name <- c(market_lookup_name, market$name)
        market_lookup_id <- c(market_lookup_id, market$id)
        
        if (is.null(market$handicap)) {
            handicaps <- c(handicaps, NA)
        } else {
            handicaps <- c(handicaps, market$handicap)
        }
    }
    
    # Loop through the prices
    for (price in match$prices) {
        fractional_odds <- price$odds$numerator / price$odds$denominator
        decimal_odds <- fractional_odds + 1
        prices <- c(prices, decimal_odds)
    }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name, handicaps = handicaps)

# Create market dataframe
market_df <- data.frame(event_id = event_ids, market_id = market_id, entrants = entrants, entrant_id = entrant_ids, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <- market_df |> select(event_id, market_id, market_name, entrants, entrant_id, handicaps, price)

# Add match names
market_df <-
    market_df |> 
    left_join(df[,c("event_name", "event_id")], by = c("event_id" = "event_id")) |> 
    relocate(event_name, .before = event_id) |> 
    rename(match_name = event_name) |> 
    select(-event_id)

# Create event ID df
event_ids_df <-
    df |>
    select(event_name, event_id) |> 
    rename(match_name = event_name) |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |> 
    mutate(match_name = paste(home_team, "v", away_team, sep = " ")) |> 
    select(match = match_name, event_id)

##%######################################################%##
#                                                          #
####               Get Head to Head Data                ####
#                                                          #
##%######################################################%##

# Filter to only include head to head markets
h2h_data <-
    market_df |> 
    filter(market_name == "Head To Head") |> 
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, home_team, home_win = price) |> 
    mutate(home_team = fix_team_names(home_team))

# Away teams
away_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == away_team) |> 
    select(match = match_name, away_team, away_win = price) |> 
    mutate(away_team = fix_team_names(away_team))

# Merge home and away teams
h2h_data <-
    home_teams |> 
    left_join(away_teams, by = c("match")) |> 
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    mutate(margin = round(1 / home_win + 1 / away_win, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Head To Head") |> 
    select(match, market = market_name, home_team, away_team, home_win, away_win, margin, agency)

##%######################################################%##
#                                                          #
####                     Touchdowns                     ####
#                                                          #
##%######################################################%##

# Alt Touchdowns
player_alt_touchdowns <-
    market_df |>
    filter(str_detect(market_name, "Anytime Touchdown Scorer")) |>
    mutate(line = 0.5) |>
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
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
    mutate(agency = "Neds") |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market = "Player Touchdowns",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Neds"
    )

##%######################################################%##
#                                                          #
####                Player Passing Yards                ####
#                                                          #
##%######################################################%##

# Alt Passing Yards
player_alt_passing_yards <-
    market_df |>
    filter(str_detect(market_name, "QB Passing Yards -")) |>
    mutate(line = str_extract(entrants, "[0-9]{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(player_name = str_remove(market_name, "QB Passing Yards - ")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(player_teams_qb) |>
    mutate(agency = "Neds") |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Passing Yards",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )

# Over Line Passing Yards
player_over_passing_yards <-
    market_df |>
    filter(str_detect(market_name, "QB Passing Yards O/U")) |>
    filter(str_detect(entrants, "Over")) |>
    mutate(line = handicaps) |>
    mutate(player_name = str_remove(market_name, "QB Passing Yards O/U - ")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(player_teams_qb) |>
  select(match_name, player_name, player_team, line, over_price = price)

# Under Line Passing Yards
player_under_passing_yards <-
    market_df |>
    filter(str_detect(market_name, "QB Passing Yards O/U")) |>
    filter(str_detect(entrants, "Under")) |>
    mutate(line = handicaps) |>
    mutate(player_name = str_remove(market_name, "QB Passing Yards O/U - ")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(player_teams_qb) |>
  select(match_name, player_name, player_team, line, under_price = price)

# Combine
player_passing_yards_over_under <-
  player_over_passing_yards |>
  left_join(player_under_passing_yards) |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(market_name = "Passing Yards") |>
  select(
    match,
    home_team,
    away_team,
    market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  )

# Combine Line with alt lines
player_passing_yards <-
  player_alt_passing_yards |>
  bind_rows(player_passing_yards_over_under) |>
  select(
    match,
    home_team,
    away_team,
    market = market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(match, player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####                 Passing Touchdowns                 ####
#                                                          #
##%######################################################%##

# Alt Passing Touchdowns
player_passing_tds <-
  market_df |>
  filter(str_detect(market_name, "QB Touchdown Passes -")) |>
  mutate(line = str_extract(entrants, "[0-9]{1}")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(market_name, "QB Touchdown Passes - ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb) |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  transmute(
    match,
    home_team,
    away_team,
    market = "Passing Touchdowns",
    player_name,
    player_team,
    opposition_team,
    line,
    over_price = price,
    agency = "Neds"
  )

##%######################################################%##
#                                                          #
####                  Passing Attempts                  ####
#                                                          #
##%######################################################%##

# Over Line Passing Attempts
player_over_passing_attempts <-
  market_df |>
  filter(str_detect(market_name, "Passing attempts")) |>
  filter(str_detect(entrants, "Over")) |>
  mutate(line = str_extract(entrants, "\\d+\\d?\\.\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(market_name, " : Passing attempts .*$")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb) |>
  select(match_name, player_name, player_team, line, over_price = price)

# Under Line Passing Attempts
player_under_passing_attempts <-
  market_df |>
  filter(str_detect(market_name, "Passing attempts")) |>
  filter(str_detect(entrants, "Under")) |>
  mutate(line = str_extract(entrants, "\\d+\\d?\\.\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(market_name, " : Passing attempts .*$")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb) |>
  select(match_name, player_name, player_team, line, under_price = price)

# Combine
player_passing_attempts <-
  player_over_passing_attempts |>
  left_join(player_under_passing_attempts) |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(market_name = "Passing Attempts") |>
  select(
    match,
    home_team,
    away_team,
    market = market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  )

##%######################################################%##
#                                                          #
####                   Rushing Yards                    ####
#                                                          #
##%######################################################%##

# Alt Rushing Yards
player_alt_rushing_yards <-
  market_df |>
  filter(str_detect(market_name, "RB Rushing Yards -")) |>
  mutate(line = str_extract(entrants, "[0-9]{1,3}")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(market_name, "RB Rushing Yards - ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr),
            by = "player_name") |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Rushing Yards",
    player_name,
    player_team,
    opposition_team,
    line,
    over_price = price,
    agency = "Neds"
  )

# Over Line Rushing Yards
player_over_rushing_yards <-
  market_df |>
  filter(str_detect(market_name, "Rushing Yards O/U")) |>
  filter(str_detect(entrants, "Over")) |>
  mutate(line = handicaps) |>
  mutate(player_name = str_remove(market_name, "Rushing Yards O/U - ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr),
            by = "player_name") |>
  select(match_name, player_name, player_team, line, over_price = price)

# Under Line Rushing Yards
player_under_rushing_yards <-
  market_df |>
  filter(str_detect(market_name, "Rushing Yards O/U")) |>
  filter(str_detect(entrants, "Under")) |>
  mutate(line = handicaps) |>
  mutate(player_name = str_remove(market_name, "Rushing Yards O/U - ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr),
            by = "player_name") |>
  select(match_name, player_name, player_team, line, under_price = price)

# Combine
player_rushing_yards_over_under <-
  player_over_rushing_yards |>
  left_join(player_under_rushing_yards) |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(market_name = "Rushing Yards") |>
  select(
    match,
    home_team,
    away_team,
    market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  )

# Combine Line with alt lines
player_rushing_yards <-
  player_alt_rushing_yards |>
  bind_rows(player_rushing_yards_over_under) |>
  select(
    match,
    home_team,
    away_team,
    market = market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(match, player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####                  Receiving Yards                   ####
#                                                          #
##%######################################################%##

# Alt Receiving Yards
player_alt_receiving_yards <-
  market_df |>
  filter(str_detect(market_name, "^To Have .* Receiving Yards$")) |>
  mutate(line = str_extract(market_name, "[0-9]{1,3}")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(entrants, " \\(.*$")) |>
  mutate(player_name = fix_player_names(player_name)) |>
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
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Receiving Yards",
    player_name,
    player_team,
    opposition_team,
    line,
    over_price = price,
    agency = "Neds"
  )

# Over Line Receiving Yards
player_over_receiving_yards <-
  market_df |>
  filter(str_detect(market_name, "Receiving Yards O/U")) |>
  filter(str_detect(entrants, "Over")) |>
  mutate(line = handicaps) |>
  mutate(player_name = str_remove(market_name, "Receiving Yards O/U - ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
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
  select(match_name, player_name, player_team, line, over_price = price)

# Under Line Receiving Yards
player_under_receiving_yards <-
  market_df |>
  filter(str_detect(market_name, "Receiving Yards O/U")) |>
  filter(str_detect(entrants, "Under")) |>
  mutate(line = handicaps) |>
  mutate(player_name = str_remove(market_name, "Receiving Yards O/U - ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
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
  select(match_name, player_name, player_team, line, under_price = price)

# Combine
player_receiving_yards_over_under <-
  player_over_receiving_yards |>
  left_join(player_under_receiving_yards) |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(market_name = "Receiving Yards") |>
  select(
    match,
    home_team,
    away_team,
    market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  )

# Combine Line with alt lines
player_receiving_yards <-
  player_alt_receiving_yards |>
  bind_rows(player_receiving_yards_over_under) |>
  select(
    match,
    home_team,
    away_team,
    market = market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(match, player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####                     Receptions                     ####
#                                                          #
##%######################################################%##

# Over Line Receptions
player_over_receptions <-
  market_df |>
  filter(str_detect(market_name, "Receptions made")) |>
  filter(str_detect(entrants, "Over")) |>
  mutate(line = str_extract(entrants, "\\d+\\d?\\.\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(market_name, " : Receptions made .*$")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = fix_player_names(player_name)) |>
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
  select(match_name, player_name, player_team, line, over_price = price)

# Under Line Receptions
player_under_receptions <-
  market_df |>
  filter(str_detect(market_name, "Receptions made")) |>
  filter(str_detect(entrants, "Under")) |>
  mutate(line = str_extract(entrants, "\\d+\\d?\\.\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(market_name, " : Receptions made .*$")) |>
  mutate(player_name = str_remove(player_name, " \\(.*$")) |>
  mutate(player_name = fix_player_names(player_name)) |>
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
  select(match_name, player_name, player_team, line, under_price = price)

# Combine
player_receptions <-
  player_over_receptions |>
  left_join(player_under_receptions) |>
  mutate(agency = "Neds") |>
  separate(match_name,
           c("home_team", "away_team"),
           sep = " vs ",
           remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(market_name = "Receptions") |>
  select(
    match,
    home_team,
    away_team,
    market = market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  )

##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
player_passing_yards |> write_csv("Data/scraped_odds/neds_player_passing_yards.csv")
player_passing_tds |> write_csv("Data/scraped_odds/neds_player_passing_tds.csv")
player_passing_attempts |> write_csv("Data/scraped_odds/neds_player_passing_attempts.csv")
player_rushing_yards |> write_csv("Data/scraped_odds/neds_player_rushing_yards.csv")
player_receiving_yards |> write_csv("Data/scraped_odds/neds_player_receiving_yards.csv")
player_receptions |> write_csv("Data/scraped_odds/neds_player_receptions.csv")
player_alt_touchdowns |> write_csv("Data/scraped_odds/neds_player_touchdowns.csv")
