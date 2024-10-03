# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

#===============================================================================
# Get JSON for each match
#===============================================================================

# Read in df
df <- read_csv("OddsScraper/EPL/Neds/neds_epl_match_urls.csv")

# Get match json files
json_match_files <- list.files("OddsScraper/EPL/Neds/", pattern = "^data_.*.json", full.names = TRUE)

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
    filter(market_name == "Match Result") |> 
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, home_team, home_win = price) |> 
    mutate(home_team = fix_team_names(home_team))

# Draw
draws <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == "Draw") |> 
    select(match = match_name, draw = price)

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
    separate(match_name,
             c("home_team", "away_team"),
             sep = " vs ",
             remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
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

##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

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
