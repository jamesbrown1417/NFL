# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)
library(nflreadr)
source("Scripts/constants.R")
source("Scripts/fix_team_names.R")
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

# Fix helpers are sourced above

# URL to get responses
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=1751"

# Make request and get response
betright_response <-
  request(betright_url) |>
  req_perform() |> 
  resp_body_json()

# Get matches
matches <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Keep only matches
matches <-
  matches |> 
  keep(~ .x$masterEventClassName == "Matches")

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$eventName
  market_propositions = market$outcomeName
  market_prices = market$price
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$masterEventName
  match_start_time = matches$minAdvertisedStartTime
  match_id = matches$masterEventId
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    match_id = match_id,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices
  )
}

# Map functions to data
all_betright_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betright_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market = market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Fix team names
betright_head_to_head_markets <-
  betright_head_to_head_markets |> 
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Rushing Yards O/U
player_rushing_over_under_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G708&format=json")

# Player Receiving Yards O/U
player_receiving_over_under_links <-
glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G709&format=json")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
  
  # Get response
  response <-
    request(link) |>
    req_perform() |> 
    resp_body_json()
  
  # Empty vectors to append to
  event_name <- c()
  event_id <- c()
  outcome_title <- c()
  outcome_name <- c()
  outcome_id <- c()
  group_by_header <- c()
  fixed_market_id <- c()
  price <- c()
  
  for (event in response$events) {
    for (outcome in event$outcomes) {
      event_name <- c(event_name, event$eventName)
      event_id <- c(event_id, event$eventId)
      outcome_title <- c(outcome_title, outcome$eventName)
      outcome_name <- c(outcome_name, outcome$outcomeName)
      outcome_id <- c(outcome_id, outcome$outcomeId)
      group_by_header <- c(group_by_header, outcome$groupByHeader)
      fixed_market_id <- c(fixed_market_id, outcome$fixedMarketId)
      price <- c(price, outcome$price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    event_id = event_id,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    outcome_id = outcome_id,
    group_by_header = group_by_header,
    fixed_market_id = fixed_market_id,
    price = price,
    link
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get player rushing yards data--------------------------------------------------------

# Match names to join
match_names <-
  all_betright_markets |>
  distinct(match, match_id)

# All Data

# Rushing Yards O/U
betright_player_rushing_yards_over_under_all <- tryCatch({
  
  map(player_rushing_over_under_links, safe_get_prop_data) |>
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |>
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |>
    left_join(match_names) |>
    filter(!is.na(outcome_name))
  
}, error = function(e) {
  NULL  # This will assign NULL to 'betright_player_points_over_under_all' if an error occurs
})

# Combine
# betright_player_points_all <-
#  bind_rows(
#    betright_player_points_alternate,
#    betright_player_points_over_under_all
#  )

# Get Overs (over under markets)
betright_player_rushing_yards_overs <-
  betright_player_rushing_yards_over_under_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(
    match,
    player_name,
    line,
    over_price,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
    fixed_market_id
  )

# Get Unders (over under markets)
betright_player_rushing_yards_unders <-
  betright_player_rushing_yards_over_under_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(
    match,
    player_name,
    line,
    under_price,
    group_by_header,
    event_id,
    outcome_name_under = outcome_name,
    outcome_id_under = outcome_id,
    fixed_market_id_under = fixed_market_id
  )

# # Get alternate player points markets
# betright_alternate_points <-
#   betright_player_points_all |>
#   filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
#   mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
#   mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
#   mutate(line = str_remove(line, "\\+$")) |>
#   mutate(line = as.integer(line) - 0.5) |> 
#   rename(over_price = price) |> 
#   select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_rushing_yards <-
  betright_player_rushing_yards_overs |>
#  bind_rows(betright_alternate_points) |>
  left_join(betright_player_rushing_yards_unders) |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Rushing Yards") |>
  separate(match,
           into = c("away_team", "home_team"),
           sep = " @ ") |>
            left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
  mutate(
    opposition_team = case_when(
      player_team == away_team ~ home_team,
      player_team == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market = "Rushing Yards",
    player_name,
    player_team,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_name_under,
    outcome_id,
    outcome_id_under,
    fixed_market_id,
    fixed_market_id_under
  )

# Get player receiving yards data--------------------------------------------------------

# Match names to join
match_names <-
  all_betright_markets |>
  distinct(match, match_id)

# All Data

# Points O/U
betright_player_receiving_yards_over_under_all <- tryCatch({
  
  map(player_receiving_over_under_links, safe_get_prop_data) |>
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |>
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |>
    left_join(match_names) |>
    filter(!is.na(outcome_name))
  
}, error = function(e) {
  NULL  # This will assign NULL to 'betright_player_points_over_under_all' if an error occurs
})

# Combine
# betright_player_points_all <-
#  bind_rows(
#    betright_player_points_alternate,
#    betright_player_points_over_under_all
#  )

# Get Overs (over under markets)
betright_player_receiving_yards_overs <-
  betright_player_receiving_yards_over_under_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(
    match,
    player_name,
    line,
    over_price,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
    fixed_market_id
  )

# Get Unders (over under markets)
betright_player_receiving_yards_unders <-
  betright_player_receiving_yards_over_under_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(
    match,
    player_name,
    line,
    under_price,
    group_by_header,
    event_id,
    outcome_name_under = outcome_name,
    outcome_id_under = outcome_id,
    fixed_market_id_under = fixed_market_id
  )

# # Get alternate player points markets
# betright_alternate_points <-
#   betright_player_points_all |>
#   filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
#   mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
#   mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
#   mutate(line = str_remove(line, "\\+$")) |>
#   mutate(line = as.integer(line) - 0.5) |> 
#   rename(over_price = price) |> 
#   select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_receiving_yards <-
  betright_player_receiving_yards_overs |>
#  bind_rows(betright_alternate_points) |>
  left_join(betright_player_receiving_yards_unders) |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Player Points") |>
  separate(match,
           into = c("away_team", "home_team"),
           sep = " @ ") |>
            left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
  mutate(
    opposition_team = case_when(
      player_team == away_team ~ home_team,
      player_team == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market = "Receiving Yards",
    player_name,
    player_team,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_name_under,
    outcome_id,
    outcome_id_under,
    fixed_market_id,
    fixed_market_id_under
  )

#===============================================================================
# Write to CSV
#===============================================================================

betright_rushing_yards |> write_csv("Data/scraped_odds/betright_player_rushing_yards.csv")
betright_receiving_yards |> write_csv("Data/scraped_odds/betright_player_receiving_yards.csv")
