# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)
<<<<<<< HEAD

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")
=======
library(nflreadr)
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

<<<<<<< HEAD
=======

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

>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b
#===============================================================================
# Get JSON for each match
#===============================================================================

# Read in df
<<<<<<< HEAD
df <- read_csv("OddsScraper/EPL/Neds/neds_epl_match_urls.csv")

# Get match json files
json_match_files <- list.files("OddsScraper/EPL/Neds/", pattern = "^data_.*.json", full.names = TRUE)
=======
df <- read_csv("OddsScraper/Neds/neds_match_urls.csv")

# Get match json files
json_match_files <- list.files("OddsScraper/Neds/", pattern = "^data_.*.json", full.names = TRUE)
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b

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
<<<<<<< HEAD
    filter(market_name == "Match Result") |> 
=======
    filter(market_name == "Head To Head") |> 
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, home_team, home_win = price) |> 
    mutate(home_team = fix_team_names(home_team))

<<<<<<< HEAD
# Draw
draws <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == "Draw") |> 
    select(match = match_name, draw = price)

=======
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b
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
<<<<<<< HEAD
    left_join(draws, by = "match") |>
    left_join(away_teams, by = c("match")) |> 
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    mutate(margin = round(1 / home_win + 1/draw + 1 / away_win, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Head To Head") |> 
    select(match, market_name, home_team, away_team, home_win, draw, away_win, margin, agency)

##%######################################################%##
#                                                          #
####                Get Total Goals Data                ####
#                                                          #
##%######################################################%##

# Filter to only include total goals markets
total_goals_data <-
    market_df |> 
    filter(str_detect(market_name, "^Over/Under Total Goals [0-9\\.]{3}$")) |>
    select(-market_name)

# Overs
total_goals_overs <-
    total_goals_data |>
=======
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
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
<<<<<<< HEAD
    filter(str_detect(entrants, "Over")) |>
    select(
        match,
        home_team,
        away_team,
        line = handicaps,
        over_price = price,
        entrant_id
    )

# Unders
total_goals_unders <-
    total_goals_data |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    filter(str_detect(entrants, "Under")) |>
    select(
        match,
        home_team,
        away_team,
        line = handicaps,
        under_price = price,
        entrant_id_under = entrant_id
    )

# Merge overs and unders
total_goals_data <-
    total_goals_overs |>
    left_join(total_goals_unders) |> 
    mutate(margin = round(1 / over_price + 1 / under_price, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Total Goals") |>
    select(match, market_name, home_team, away_team, line, over_price, under_price, margin, agency)

##%######################################################%##
#                                                          #
####                Get Team Goals Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include team total goals markets
team_total_goals_data <-
    market_df |> 
    filter(str_detect(market_name, "^Over/Under [A-Za-z]+ Total Goals [0-9\\.]{3}$"))

# Overs
team_total_goals_overs <-
    team_total_goals_data |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    filter(str_detect(entrants, "Over")) |>
    mutate(team = str_remove(market_name, "Over\\/Under")) |> 
    mutate(team = str_remove(team, " Total Goals.*$")) |> 
    select(
        match,
        home_team,
        away_team,
        team,
        line = handicaps,
        over_price = price,
        entrant_id
    )

# Unders
team_total_goals_unders <-
    team_total_goals_data |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    filter(str_detect(entrants, "Under")) |>
    mutate(team = str_remove(market_name, "Over\\/Under")) |> 
    mutate(team = str_remove(team, " Total Goals.*$")) |> 
    select(
        match,
        home_team,
        away_team,
        team,
        line = handicaps,
        under_price = price,
        entrant_id_under = entrant_id
    )

# Merge overs and unders
team_total_goals_data <-
    team_total_goals_overs |>
    left_join(team_total_goals_unders) |> 
    mutate(margin = round(1 / over_price + 1 / under_price, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Total Goals") |>
    select(match, market_name, home_team, away_team, team, line, over_price, under_price, margin, agency)

##%######################################################%##
#                                                          #
####                Both Teams To Score                 ####
#                                                          #
##%######################################################%##

# Filter to only include both teams to score markets
btts_data <-
    market_df |> 
    filter(str_detect(market_name, "^Both Teams to Score$")) |>
    select(-market_name)

# Yes
btts_yes <-
    btts_data |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    filter(str_detect(entrants, "Yes")) |>
    select(
        match,
        home_team,
        away_team,
        yes_price = price,
        entrant_id
    )

# No
btts_no <-
    btts_data |>
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    filter(str_detect(entrants, "No")) |>
    select(
        match,
        home_team,
        away_team,
        no_price = price,
        entrant_id_under = entrant_id
    )

# Merge yes and no
btts_data <-
    btts_yes |>
    left_join(btts_no) |> 
    mutate(margin = round(1 / yes_price + 1 / no_price, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Both Teams to Score") |>
    select(match, market_name, home_team, away_team, yes_price, no_price, margin, agency)

##%######################################################%##
#                                                          #
####              Player Attempted Passes               ####
#                                                          #
##%######################################################%##

# Filter to only include player attempted passes markets
player_attempted_passes_data <-
    market_df |>
    filter(str_detect(market_name, "To Have .* Passes")) |>
    mutate(line = str_extract(market_name, "[0-9]+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |>
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
        market_name = "Player Attempted Passes",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )

##%######################################################%##
#                                                          #
####                    Player Shots                    ####
#                                                          #
##%######################################################%##

# Filter to only include player shots markets
player_shots_data <-
    market_df |>
    filter(str_detect(market_name, "To Have .* Shots$")) |>
    mutate(line = str_extract(market_name, "[0-9]+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |>
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
        market_name = "Player Shots",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )

##%######################################################%##
#                                                          #
####               Player Shots On Target               ####
#                                                          #
##%######################################################%##

# Filter to only include player shots_on_target markets
player_shots_on_target_data <-
    market_df |>
    filter(str_detect(market_name, "To Have .* Shots on Target$")) |>
    mutate(line = str_extract(market_name, "[0-9]+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |>
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
        market_name = "Player Shots On Target",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )

##%######################################################%##
#                                                          #
####                   Player Tackles                   ####
#                                                          #
##%######################################################%##

# Filter to only include player tackles markets
player_tackles_data <-
    market_df |>
    filter(str_detect(market_name, "To Have .* Tackles$")) |>
    mutate(line = str_extract(market_name, "[0-9]+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |>
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
        market_name = "Player Tackles",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )

##%######################################################%##
#                                                          #
####                    Player Goals                    ####
#                                                          #
##%######################################################%##

# Filter to only include player goals markets
player_goals_data <-
    market_df |>
    filter(
        str_detect(
            market_name,
            "Anytime Goalscorer|Player to Score 2 or More Goals|Hat-trick"
        )
    ) |>
    mutate(line = case_when(
        str_detect(market_name, "Anytime") ~ 0.5,
        str_detect(market_name, "2 or More") ~ 1.5,
        str_detect(market_name, "Hat-trick") ~ 2.5
    )) |>
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |>
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
        market_name = "Player Goals",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )

##%######################################################%##
#                                                          #
####                   Player Assists                   ####
#                                                          #
##%######################################################%##

# Filter to only include player assists markets
player_assists_data <-
    market_df |>
    filter(str_detect(market_name, "^Anytime Assist$")) |>
    mutate(line = 0.5) |> 
    mutate(player_name = str_remove(entrants, " \\(.*$")) |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(epl_squads) |>
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
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency =  "Neds"
    )
=======
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
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b

##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

<<<<<<< HEAD
h2h_data |> write_csv("Data/scraped_odds/EPL/neds_h2h.csv")
total_goals_data |> write_csv("Data/scraped_odds/EPL/neds_total_goals.csv")
team_total_goals_data |> write_csv("Data/scraped_odds/EPL/neds_team_total_goals.csv")
btts_data |> write_csv("Data/scraped_odds/EPL/neds_btts.csv")
player_attempted_passes_data |> write_csv("Data/scraped_odds/EPL/neds_player_attempted_passes.csv")
player_shots_data |> write_csv("Data/scraped_odds/EPL/neds_player_shots.csv")
player_shots_on_target_data |> write_csv("Data/scraped_odds/EPL/neds_player_shots_on_target.csv")
player_tackles_data |> write_csv("Data/scraped_odds/EPL/neds_player_tackles.csv")
player_goals_data |> write_csv("Data/scraped_odds/EPL/neds_player_goals.csv")
player_assists_data |> write_csv("Data/scraped_odds/EPL/neds_player_assists.csv")
=======
h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
player_passing_yards |> write_csv("Data/scraped_odds/neds_player_passing_yards.csv")
player_passing_tds |> write_csv("Data/scraped_odds/neds_player_passing_tds.csv")
player_passing_attempts |> write_csv("Data/scraped_odds/neds_player_passing_attempts.csv")
player_rushing_yards |> write_csv("Data/scraped_odds/neds_player_rushing_yards.csv")
player_receiving_yards |> write_csv("Data/scraped_odds/neds_player_receiving_yards.csv")
player_receptions |> write_csv("Data/scraped_odds/neds_player_receptions.csv")
player_alt_touchdowns |> write_csv("Data/scraped_odds/neds_player_touchdowns.csv")
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b
