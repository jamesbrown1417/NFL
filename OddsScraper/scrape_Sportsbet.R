# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/american-football/nfl"

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

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
  # Get data from main market page
  matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantRow_fklqmim") |>
      html_text()
      
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Function to get odds
  get_odds <- function(match) {
    odds <-
      match |>
      html_nodes(".priceTextSize_frw9zm9") |>
      html_text() |>
      as.numeric()
    
    # Home team
    home_win <- odds[2]
    away_win <- odds[1]
    
    # Output
    tibble(home_win, away_win)
  }
  
  # Function to get start time
  get_start_time <- function(match) {
    start_time <-
      match |>
      html_nodes(".oneLine_f15ay66x") |>
      html_text()
    
    # Output
    tibble(start_time)
  }
  
  # Map functions to each match and combine together
  all_main_market_data <-
    bind_cols(
      map(matches, get_team_names) |> bind_rows(),
      map(matches, get_odds) |> bind_rows(),
      map(matches, get_start_time) |> bind_rows()
    )
  
  #===============================================================================
  # Head to Head markets---------------------------------------------------------#
  #===============================================================================
  
  sportsbet_h2h <-
    all_main_market_data |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet")
  
  # Write to csv
  write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantRow_fklqmim") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Get match links
  match_links <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".linkMultiMarket_fcmecz0") |>
    html_attr("href")
  
  # Get match IDs from links
  match_ids <-
    match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()
  
  # Get data from main market page
  matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Keep only the first 16 matches
  match_ids <- match_ids[1:16]
  matches <- matches[1:16]
  
  # Get team names that correspond to each match link
  team_names <-
    map_dfr(matches, get_team_names) |>
    bind_cols("match_id" = match_ids) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team))
  
  # Match info links
  match_info_links <- glue(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true"
  )
  
  # Top Markets
  top_market_links <- glue(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/211/Markets"
  )
  
  # Quarterback Prop Markets
  qb_prop_links <- glue(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/642/Markets"
  )
  
  # Rushing Prop Markets
  rushing_prop_links <- glue(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/643/Markets"
  )
  
  # Receiving Prop Markets
  receiving_prop_links <- glue(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/644/Markets"
  )
  
  # Total Markets
  total_market_links <- glue(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/220/Markets"
  )
  
  # Get IDs needed for SGM engine-------------------------------------------------
  read_prop_url_metadata <- function(url) {
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Empty vectors to append to
    class_external_id = c()
    competition_external_id = c()
    event_external_id = c()
    
    # Append to vectors
    class_external_id = c(class_external_id, sb_response$classExternalId)
    competition_external_id = c(competition_external_id,
                                sb_response$competitionExternalId)
    event_external_id = c(event_external_id, sb_response$externalId)
    
    # Output
    tibble(class_external_id,
           competition_external_id,
           event_external_id,
           url) |>
      mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
      rename(match_id = url) |>
      mutate(match_id = as.numeric(match_id))
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)
  
  # Map function to player points urls
  player_prop_metadata <-
    map(match_info_links, safe_read_prop_metadata)
  
  # Get just result part from output
  player_prop_metadata <-
    player_prop_metadata |>
    map("result") |>
    map_df(bind_rows)
  
  # Function to read a url and get the player props-------------------------------
  
  read_prop_url <- function(url) {
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
      for (selection in market$selections) {
        # Append to vectors
        prop_market_name = c(prop_market_name, market$name)
        selection_name_prop = c(selection_name_prop, selection$name)
        prop_market_selection = c(prop_market_selection, selection$resultType)
        prop_market_price = c(prop_market_price, selection$price$winPrice)
        player_id = c(player_id, selection$externalId)
        market_id = c(market_id, market$externalId)
        if (is.null(selection$unformattedHandicap)) {
          selection$unformattedHandicap = NA
          handicap = c(handicap, selection$unformattedHandicap)
        } else {
          selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
          handicap = c(handicap, selection$unformattedHandicap)
        }
      }
    }
    
    # Output
    tibble(
      prop_market_name,
      selection_name_prop,
      prop_market_selection,
      prop_market_price,
      player_id,
      market_id,
      handicap,
      url
    )
    
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)
  
  #===========================================================================
  # Top Markets
  #===========================================================================
  
  # Map function to top market urls]
  top_market_data <-
    map(top_market_links, safe_read_prop_url)
  
  # Get just result part from output
  top_market_data <-
    top_market_data |>
    map("result") |>
    map_df(bind_rows)
  
  # Add market name
  top_market_data <-
    top_market_data |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_prop_metadata)
  
  #===========================================================================
  # Quarterback Prop Markets
  #===========================================================================
  
  # Map function to shots urls
  qb_prop_data <-
    map(qb_prop_links, safe_read_prop_url)
  
  # Get just result part from output
  qb_prop_data <-
    qb_prop_data |>
    map("result") |>
    map_df(bind_rows)
  
  # If nrow 0 create tibble with 0 rows
  if (nrow(qb_prop_data) == 0) {
    qb_prop_data <-
      tibble(match = NA,
             prop_market_name = NA,
             selection_name_prop = NA,
             prop_market_price = NA,
             market_id = NA,
             player_id = NA,
             class_external_id = NA,
             competition_external_id = NA,
             event_external_id = NA,
             url = NA)
  }
  
  # Add market name
  qb_prop_data <-
    qb_prop_data |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_prop_metadata)
  
  # Passing Yards---------------------------------------------------------------
  passing_yards <-
    qb_prop_data |>
    filter(str_detect(prop_market_name, "\\- Passing Yds$|\\- Alt Passing Yds$"))
  
  # Overs
  passing_yards_overs <-
    passing_yards |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Passing Yards") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  passing_yards_unders <-
    passing_yards |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Passing Yards") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  passing_yards_all <-
    passing_yards_overs |>
    left_join(passing_yards_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    left_join(player_teams_qb, by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  #===========================================================================
  # Write all to CSV
  #===========================================================================
  
  # Both Teams To Score
  write_csv(both_teams_to_score_all, "Data/scraped_odds/EPL/sportsbet_both_teams_to_score.csv")
}

# Run Functions
main_markets_function()
player_props_function()
