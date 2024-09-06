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

pointsbet_h2h_main <- function() {
  # URL of website
  pointsbet_url = "https://api.pointsbet.com/api/v2/competitions/11444/events/featured?includeLive=false&page=1"
  
  # Make request and get response
  pointsbet_response <-
    request(pointsbet_url) |>
    req_perform() |>
    resp_body_json()
  
  # List of matches and data
  events <- pointsbet_response$events
  
  # Loop through to get all data--------------------------------------------------
  
  # Create empty vectors
  match_names <- c()
  match_starts_at <- c()
  home_teams <- c()
  away_teams <- c()
  event_names <- c()
  outcome_names <- c()
  outcome_prices <- c()
  keys <- c()
  
  # Loop through events
  for (match in events) {
    for (market in match$specialFixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, match$name)
        match_starts_at <- c(match_starts_at, match$startsAt)
        home_teams <- c(home_teams, match$homeTeam)
        away_teams <- c(away_teams, match$awayTeam)
        event_names <- c(event_names, market$eventName)
        outcome_names <- c(outcome_names, outcome$name)
        outcome_prices <- c(outcome_prices, outcome$price)
        keys <- c(keys, match$key)
      }
    }
  }
  
  # Output tibble
  pointsbet_data <-
    tibble(
      match = match_names,
      start_time = match_starts_at,
      home_team = home_teams,
      away_team = away_teams,
      event = event_names,
      outcome = outcome_names,
      price = outcome_prices
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    relocate(match, .before = start_time) |>
    mutate(across(everything(), str_squish))
  
  #===============================================================================
  # Head to head markets
  #===============================================================================
  
  # Filter to head to head markets
  pointsbet_data_h2h <-
    pointsbet_data |>
    filter(event == "Moneyline")
  
  # Home Teams
  pointsbet_data_h2h_home <-
    pointsbet_data_h2h |>
    filter(home_team == outcome) |>
    select(match,
           start_time,
           market = event,
           home_team,
           home_win = price) |>
    mutate(home_team = fix_team_names(home_team))
  
  # Away Teams
  pointsbet_data_h2h_away <-
    pointsbet_data_h2h |>
    filter(away_team == outcome) |>
    select(match,
           start_time,
           market = event,
           away_team,
           away_win = price) |>
    mutate(away_team = fix_team_names(away_team))
  
  # Combine
  pointsbet_h2h <-
    full_join(
      pointsbet_data_h2h_home,
      pointsbet_data_h2h_away,
      by = c("match", "start_time", "market")
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market = "Head To Head") |>
    select(match,
           start_time,
           market_name = market,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(home_win = as.numeric(home_win),
           away_win = as.numeric(away_win)) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Pointsbet")
  
  # Write to csv
  write_csv(pointsbet_h2h, "Data/scraped_odds/pointsbet_h2h.csv")
  
  #===============================================================================
  # Player Props
  #===============================================================================
  
  # Get unique keys
  keys <- unique(keys)
  
  # Get each match's api page
  match_urls <-
    paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)
  
  # Create a function that gets the player props from each URL
  get_player_props <- function(url) {
    # Make request and get response
    pointsbet_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Loop through to get prop data---------------------------------------------
    
    # Create empty vectors
    match_names <- c()
    market_names <- c()
    outcome_names <- c()
    outcome_types <- c()
    outcome_prices <- c()
    headers <- c()
    event_key <- c()
    market_key <- c()
    outcome_key <- c()
    
    # Loop through events
    for (market in pointsbet_response$fixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, pointsbet_response$name)
        
        if (!is.null(market$name)) {
          market_names <- c(market_names, market$name)
        } else {
          market_names <- c(market_names, NA)
        }
        
        if (!is.null(outcome$name)) {
          outcome_names <- c(outcome_names, outcome$name)
        } else {
          outcome_names <- c(outcome_names, NA)
        }
        
        if (!is.null(outcome$groupByHeader)) {
          headers <- c(headers, outcome$groupByHeader)
        } else {
          headers <- c(headers, NA)
        }
        
        if (!is.null(outcome$outcomeType)) {
          outcome_types <- c(outcome_types, outcome$outcomeType)
        } else {
          outcome_types <- c(outcome_types, NA)
        }
        
        if (!is.null(outcome$price)) {
          outcome_prices <- c(outcome_prices, outcome$price)
        } else {
          outcome_prices <- c(outcome_prices, NA)
        }
        
        event_key <- c(event_key, pointsbet_response$key)
        
        if (!is.null(market$key)) {
          market_key <- c(market_key, market$key)
        } else {
          market_key <- c(market_key, NA)
        }
        
        if (!is.null(outcome$key)) {
          outcome_key <- c(outcome_key, outcome$key)
        } else {
          outcome_key <- c(outcome_key, NA)
        }
      }
    }
    
    # Output tibble
    tibble(
      match = match_names,
      market = market_names,
      headers = headers,
      outcome = outcome_names,
      outcome_type = outcome_types,
      price = outcome_prices,
      EventKey = event_key,
      MarketKey = market_key,
      OutcomeKey = outcome_key
    )
  }
  
  # Map function to each URL
  pointsbet_data_player_props <- map_df(match_urls, get_player_props)
  
  #===============================================================================
  # Touchdowns
  #===============================================================================
  
  # Filter to touchdown markets
  pointsbet_player_touchdowns <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Score .* Touchdowns \\(|Anytime Touchdown Scorer \\("))
  
  # Get Overs
  pointsbet_player_touchdowns_all <-
    pointsbet_player_touchdowns |>
    filter(str_detect(market, "Anytime|\\+")) |>
    mutate(line = as.numeric(str_extract(market, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(market, "Anytime"), 0.5, line - 0.5)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = fix_player_names(outcome)) |> 
    mutate(market = "Touchdowns") |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    ) |> 
    mutate(agency = "Pointsbet") |> 
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
      over_price,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey    ) |>
    distinct(match, player_name, line, over_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Passing Yards
  #===============================================================================
  
  # Filter to passing yards
  pointsbet_player_passing_yards <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Passing Yards|Passing Yds"))
  
  # Get Overs
  pointsbet_player_passing_yards_overs <-
    pointsbet_player_passing_yards |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Passing Yards") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_passing_yards_unders <-
    pointsbet_player_passing_yards |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Passing Yards") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_passing_yards_all <-
    pointsbet_player_passing_yards_overs |>
    left_join(pointsbet_player_passing_yards_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
    left_join(player_teams_qb, by = "player_name") |>
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Passing Touchdowns
  #===============================================================================
  
  # Filter to passing touchdowns
  pointsbet_player_passing_tds <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Passing Touchdowns|Passing TDs"))
  
  # Get Overs
  pointsbet_player_passing_tds_overs <-
    pointsbet_player_passing_tds |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Passing Touchdowns") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_passing_tds_unders <-
    pointsbet_player_passing_tds |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Passing Touchdowns") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_passing_tds_all <-
    pointsbet_player_passing_tds_overs |>
    left_join(pointsbet_player_passing_tds_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
    left_join(player_teams_qb, by = "player_name") |>
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Passing Attempts
  #===============================================================================
  
  # Filter to passing attempts
  pointsbet_player_passing_attempts <-
    pointsbet_data_player_props |>
    filter(str_detect(market, " Pass Attempts"))
  
  # Get Overs
  pointsbet_player_passing_attempts_overs <-
    pointsbet_player_passing_attempts |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Passing Attempts") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_passing_attempts_unders <-
    pointsbet_player_passing_attempts |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Passing Attempts") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_passing_attempts_all <-
    pointsbet_player_passing_attempts_overs |>
    left_join(pointsbet_player_passing_attempts_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
    left_join(player_teams_qb, by = "player_name") |>
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Passes Completed
  #===============================================================================
  
  # Filter to passes completed
  pointsbet_player_passes_completed <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Pass Completions"))
  
  # Get Overs
  pointsbet_player_passes_completed_overs <-
    pointsbet_player_passes_completed |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Pass Completions") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_passes_completed_unders <-
    pointsbet_player_passes_completed |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Pass Completions") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_passes_completed_all <-
    pointsbet_player_passes_completed_overs |>
    left_join(pointsbet_player_passes_completed_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
    left_join(player_teams_qb, by = "player_name") |>
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Rushing Yards
  #===============================================================================
  
  # Filter to rushing yards
  pointsbet_player_rushing_yards <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Rushing Yards|Rush Yds")) |> 
    filter(str_detect(market, "\\+", negate = TRUE))
  
  # Get Overs
  pointsbet_player_rushing_yards_overs <-
    pointsbet_player_rushing_yards |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Rushing Yards") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_rushing_yards_unders <-
    pointsbet_player_rushing_yards |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Rushing Yards") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_rushing_yards_all <-
    pointsbet_player_rushing_yards_overs |>
    left_join(pointsbet_player_rushing_yards_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr),
              by = "player_name") |>
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Rushing Attempts
  #===============================================================================
  
  # Filter to rushing attempts
  pointsbet_player_rushing_attempts <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Rushing Attempts|Rush Attempts"))
  
  # Get Overs
  pointsbet_player_rushing_attempts_overs <-
    pointsbet_player_rushing_attempts |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Rushing Attempts") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_rushing_attempts_unders <-
    pointsbet_player_rushing_attempts |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Rushing Attempts") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_rushing_attempts_all <-
    pointsbet_player_rushing_attempts_overs |>
    left_join(pointsbet_player_rushing_attempts_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
    mutate(player_name = fix_player_names(player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr),
              by = "player_name") |>
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Receiving Yards
  #===============================================================================
  
  # Filter to receiving yards
  pointsbet_player_receiving_yards <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Receiving Yards|Rec Yds")) |> 
    filter(str_detect(market, "\\+", negate = TRUE))
  
  # Get Overs
  pointsbet_player_receiving_yards_overs <-
    pointsbet_player_receiving_yards |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Receiving Yards") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_receiving_yards_unders <-
    pointsbet_player_receiving_yards |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Receiving Yards") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_receiving_yards_all <-
    pointsbet_player_receiving_yards_overs |>
    left_join(pointsbet_player_receiving_yards_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
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
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Receptions
  #===============================================================================
  
  # Filter to receptions
  pointsbet_player_receptions <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Receptions"))
  
  # Get Overs
  pointsbet_player_receptions_overs <-
    pointsbet_player_receptions |>
    filter(str_detect(outcome, "Over|\\+")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           over_price = as.numeric(price)) |>
    mutate(line = if_else(str_detect(outcome, "\\+"), line - 0.5, line)) |>
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Receptions") |>
    mutate(player_name = str_remove(outcome, " Over.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      over_price,
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_receptions_unders <-
    pointsbet_player_receptions |>
    filter(str_detect(outcome, "Under")) |>
    mutate(line = as.numeric(str_extract(outcome, "\\d+\\.?\\d?")),
           under_price = as.numeric(price)) |>
    
    separate(match, c("away_team", "home_team"), sep = " @ ") |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    mutate(market = "Receptions") |>
    mutate(player_name = str_remove(outcome, " Under.*$| \\d+.*$")) |>
    select(
      match,
      home_team,
      away_team,
      player_name,
      market,
      line,
      under_price,
      EventKey,
      MarketKey,
      OutcomeKeyUnders = OutcomeKey
    )
  
  # Join Overs and Unders
  pointsbet_player_receptions_all <-
    pointsbet_player_receptions_overs |>
    left_join(pointsbet_player_receptions_unders) |>
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Pointsbet") |>
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
    select(
      match,
      player_name,
      player_team,
      market,
      line,
      over_price,
      under_price,
      margin,
      agency,
      EventKey,
      MarketKey,
      OutcomeKey,
      OutcomeKeyUnders
    ) |>
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |>
    arrange(match, player_name, line)
  
  #===============================================================================
  # Write to CSV
  #===============================================================================
  
  pointsbet_player_touchdowns_all |> 
    write_csv("Data/scraped_odds/pointsbet_touchdowns.csv")
  
  pointsbet_player_passing_yards_all |>
    write_csv("Data/scraped_odds/pointsbet_passing_yards.csv")
  
  pointsbet_player_passes_completed_all |>
    write_csv("Data/scraped_odds/pointsbet_passing_completions.csv")
  
  pointsbet_player_passing_tds_all |>
    write_csv("Data/scraped_odds/pointsbet_passing_tds.csv")
  
  pointsbet_player_passing_attempts_all |>
    write_csv("Data/scraped_odds/pointsbet_passing_attempts.csv")
  
  pointsbet_player_passing_yards_all |>
    write_csv("Data/scraped_odds/pointsbet_passing_yards.csv")
  
  pointsbet_player_rushing_yards_all |>
    write_csv("Data/scraped_odds/pointsbet_rushing_yards.csv")
  
  pointsbet_player_rushing_attempts_all |>
    write_csv("Data/scraped_odds/pointsbet_rushing_attempts.csv")
  
  pointsbet_player_receiving_yards_all |>
    write_csv("Data/scraped_odds/pointsbet_receiving_yards.csv")
  
  pointsbet_player_receptions_all |>
    write_csv("Data/scraped_odds/pointsbet_receptions.csv")
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

# This runs both the props and head to head as they use same info
h2h_safe_pointsbet <- safely(pointsbet_h2h_main)

# Run functions
h2h_safe_pointsbet()