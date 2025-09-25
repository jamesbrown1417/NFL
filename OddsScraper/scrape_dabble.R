# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)
source("Scripts/constants.R")
source("Scripts/fix_team_names.R")

# URL to get responses
competitions_api_url = "https://api.dabble.com.au/competitions/fa66826c-2e37-4460-b21e-e7ec6974256e/sport-fixtures"

# Make request and get response
dabble_response <-
  request(competitions_api_url) |>
  req_perform() |>
  resp_body_json()

# Function to get fixture details
get_fixture_details <- function(data) {
  match <- data$name
  match_id <- data$id
  
  # Get Prices Information
  all_prices <-
    map_dfr(data$prices, ~{
      tibble(
        id = .x$id,
        marketId = .x$marketId,
        selectionId = .x$selectionId,
        price = .x$price
      )
    })
  
  # Get Markets Information
  all_markets <-
    map_dfr(data$markets, ~{
      tibble(
        market_name = .x$name,
        marketId = .x$id
      )
    })
  
  # Get Selections Information
  all_selections <-
    map_dfr(data$selections, ~{
      tibble(
        selection_name = .x$name,
        selectionId = .x$id
      )
    })
  
  # Return tibble
  all_prices |>
    left_join(all_markets) |> 
    left_join(all_selections) |> 
    mutate(match = match, id = match_id)
  
}

# Map over data
data_list <- data <- dabble_response$data
fixture_details <- map_dfr(data_list, get_fixture_details)

#===============================================================================
# Get H2H Data
#===============================================================================

all_h2h <-
  fixture_details |> 
  filter(str_detect(market_name, "Match Winner \\(Incl Overtime\\)")) |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(selection_name = fix_team_names(selection_name))

# Home Teams
home_teams <-
  all_h2h |>
  mutate(market_name = "Head To Head") |> 
  filter(home_team == selection_name) |>
  rename(home_win = price) |>
  select(match, home_team, away_team, market_name, home_win) |> 
  group_by(match) |> 
  arrange(match, desc(home_win)) |>
  slice_head(n = 1)

# Away teams
away_teams <-
  all_h2h |>
  mutate(market_name = "Head To Head") |> 
  filter(away_team == selection_name) |>
  rename(away_win = price) |>
  select(match, home_team, away_team, market_name, away_win) |> 
  group_by(match) |> 
  arrange(match, desc(away_win)) |>
  slice_head(n = 1)


# Combine
dabble_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "Dabble")

# Write to csv
write_csv(dabble_head_to_head_markets, "Data/scraped_odds/dabble_h2h.csv")

#===============================================================================
# Get Fixture Details
#===============================================================================

fixtures <-
  all_h2h |>
  distinct(match, id)

#===============================================================================
# Prop Data
#===============================================================================

# Get List of Fixture URLs
fixture_urls <- paste0("https://api.dabble.com.au/sportfixtures/details/", fixtures$id, "?filter=dfs-enabled")

# Function to get fixture details
get_fixture_details <- function(url) {
  
  # Get response from URL
  fixture_response <-
    request(url) |>
    req_perform() |>
    resp_body_json()
  
  # Get Prices Information
  all_prices <-
    map_dfr(fixture_response$data$prices, ~{
      tibble(
        id = .x$id,
        marketId = .x$marketId,
        selectionId = .x$selectionId,
        price = .x$price
      )
    })
  
  # Get Markets Information
  all_markets <-
    map_dfr(fixture_response$data$markets, ~{
      tibble(
        prop_name = .x$name,
        market_name = .x$resultingType,
        id = .x$id
      )
    })
  
  
  # Get Selections Information
  all_selections <-
    map_dfr(fixture_response$data$selections, ~{
      tibble(
        selection_name = .x$name,
        id = .x$id
      )
    })
  
  # Get Match Names
  match_name <- fixture_response$data$name
  
  # Combine together
  all_prices |>
    left_join(all_markets, by = c("marketId" = "id")) |>
    left_join(all_selections, by = c("selectionId" = "id")) |> 
    mutate(match_id = str_remove(url, "https://api.dabble.com.au/sportfixtures/details/")) |> 
    mutate(match = match_name)
}

# Map over data
prop_data <- map(fixture_urls, safely(get_fixture_details), .progress = TRUE)

# Extract successful results
prop_data <- prop_data |>
  keep(~is.null(.x$error)) |>
  map_dfr("result")

#===============================================================================
# Get Player Shots
#===============================================================================

# Filter to player shots markets
player_shots_markets <-
  prop_data |> 
  filter(market_name == "pick_em_shots") |> 
  filter(str_detect(selection_name, "Over|Under")) |> 
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_shots_markets <-
  player_shots_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(player_name = str_remove_all(player_name, " shots")) |> 
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))


# Over lines
over_lines <-
  player_shots_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player shots") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_shots_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player shots") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_shots_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Passes Attempted
#===============================================================================

# Filter to player passes attempted markets
player_passes_attempted_markets <-
  prop_data |> 
  filter(market_name == "pick_em_passes_attempted") |> 
  filter(str_detect(selection_name, "Over|Under")) |> 
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_passes_attempted_markets <-
  player_passes_attempted_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(player_name = str_remove_all(player_name, " passes attempted")) |> 
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

# Over lines
over_lines <-
  player_passes_attempted_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Passes Attempted") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_passes_attempted_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Passes Attempted") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_passes_attempted_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Goals
#===============================================================================

# Filter to player goals markets
player_goals_markets <-
  prop_data |>
  filter(str_detect(market_name, "goals")) |>
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(price = round(price*0.9, digits = 2))

# Extract player names etc from standard markets
player_goals_markets <-
  player_goals_markets |>
  mutate(player_name = str_remove(selection_name, "\\s(Over|Under).*$")) |>
  mutate(player_name = str_remove_all(player_name, " goals.*$")) |> 
  mutate(line = str_extract(selection_name, "[0-9]+(?:\\.[0-9]+)?")) |>
  mutate(line = as.numeric(line)) |>
  mutate(type = ifelse(str_detect(selection_name, "Over|\\+"), "Over", "Under")) |>
  mutate(player_name = str_trim(player_name))

# Over lines
over_lines <-
  player_goals_markets |>
  filter(type == "Over") |>
  mutate(market_name = "Player Goals") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_goals_markets |>
  filter(type == "Under") |>
  mutate(market_name = "Player Goals") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_goals_markets <-
  over_lines |>
  full_join(under_lines, by = c("match", "market_name", "player_name", "line")) |>
  select(match, market_name, player_name, line, over_price, under_price) |>
  mutate(agency = "Dabble") |>
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Shots On Target
#===============================================================================

# Filter to player shots on target markets
player_shots_on_target_markets <-
  prop_data |> 
  filter(market_name == "pick_em_shots_on_target") |> 
  filter(str_detect(selection_name, "Over|Under")) |> 
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_shots_on_target_markets <-
  player_shots_on_target_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(player_name = str_remove_all(player_name, " shots on target")) |> 
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

# Over lines
over_lines <-
  player_shots_on_target_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Shots On Target") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_shots_on_target_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Shots On Target") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_shots_on_target_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Tackles
#===============================================================================

# Filter to player tackles markets
player_tackles_markets <-
  prop_data |>
  filter(market_name == "pick_em_tackles") |>
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_tackles_markets <-
  player_tackles_markets |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |>
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |>
  mutate(player_name = str_remove_all(player_name, " tackles")) |>
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |>
  mutate(type = ifelse(type, "Over", "Under")) |>
  mutate(player_name = str_trim(player_name))

# Over lines
over_lines <-
  player_tackles_markets |>
  filter(type == "Over") |>
  mutate(market_name = "Player Tackles") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_tackles_markets |>
  filter(type == "Under") |>
  mutate(market_name = "Player Tackles") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_tackles_markets <-
  over_lines |>
  full_join(under_lines) |>
  select(match, market_name, player_name, line, over_price, under_price) |>
  mutate(agency = "Dabble") |>
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Fouls
#===============================================================================

# Filter to player fouls markets
player_fouls_markets <-
  prop_data |> 
  filter(market_name == "pick_em_fouls") |> 
  filter(str_detect(selection_name, "Over|Under")) |> 
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_fouls_markets <-
  player_fouls_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(player_name = str_remove_all(player_name, " fouls")) |> 
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

# Over lines
over_lines <-
  player_fouls_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Fouls") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_fouls_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Fouls") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_fouls_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Saves
#===============================================================================

# Filter to player saves markets
player_saves_markets <-
  prop_data |> 
  filter(market_name == "pick_em_saves") |> 
  filter(str_detect(selection_name, "Over|Under")) |> 
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_saves_markets <-
  player_saves_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(player_name = str_remove_all(player_name, " saves")) |> 
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

# Over lines
over_lines <-
  player_saves_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Saves") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_saves_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Saves") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_saves_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Assists
#===============================================================================

# Filter to player assists markets
player_assists_markets <-
  prop_data |> 
  filter(market_name == "pick_em_assists") |> 
  filter(str_detect(selection_name, "Over|Under")) |> 
  mutate(price = round(price*0.9, digits = 2))

# Extract player names
player_assists_markets <-
  player_assists_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(player_name = str_remove_all(player_name, " assists")) |> 
  mutate(line = str_extract(selection_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

# Over lines
over_lines <-
  player_assists_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Assists") |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_assists_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Assists") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_assists_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Fix team and player names-----------------------------------------------------
#===============================================================================

# Fix player names--------------------------------------------------------------

# Apply EPL player name cleaning to all markets
dabble_player_shots_markets <- dabble_player_shots_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_passes_attempted_markets <- dabble_player_passes_attempted_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_goals_markets <- dabble_player_goals_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_shots_on_target_markets <- dabble_player_shots_on_target_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_tackles_markets <- dabble_player_tackles_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_fouls_markets <- dabble_player_fouls_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_saves_markets <- dabble_player_saves_markets |> mutate(player_name = fix_player_names(player_name))
dabble_player_assists_markets <- dabble_player_assists_markets |> mutate(player_name = fix_player_names(player_name))

# Fix Team Names----------------------------------------------------------------

# Apply team name fixes to all markets
dabble_player_shots_markets <- dabble_player_shots_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_passes_attempted_markets <- dabble_player_passes_attempted_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_goals_markets <- dabble_player_goals_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_shots_on_target_markets <- dabble_player_shots_on_target_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_tackles_markets <- dabble_player_tackles_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_fouls_markets <- dabble_player_fouls_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_saves_markets <- dabble_player_saves_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

dabble_player_assists_markets <- dabble_player_assists_markets |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

dabble_player_shots_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_shots.csv")
dabble_player_passes_attempted_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_passes_attempted.csv")
dabble_player_goals_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_goals.csv")
dabble_player_shots_on_target_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_shots_on_target.csv")
dabble_player_tackles_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_tackles.csv")
dabble_player_fouls_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_fouls.csv")
dabble_player_saves_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_saves.csv")
dabble_player_assists_markets |> write_csv("Data/scraped_odds/EPL/dabble_player_assists.csv")

