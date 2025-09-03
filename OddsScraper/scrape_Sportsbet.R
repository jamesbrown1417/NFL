# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)
source("Scripts/constants.R")
source("Scripts/fix_team_names.R")

# Fallback shim: if read_html_live() isn't available, use read_html()
if (!exists("read_html_live")) {
  read_html_live <- function(x, ...) read_html(x, ...)
}

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/american-football/nfl"

# Load the live-rendered Sportsbet page once and reuse (align with AFL approach)
sportsbet_html <-
  sportsbet_url |>
  read_html_live()

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

# Get Fix Team Names Function and Player Names Function are sourced above

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
  # Get data from main market page
  matches <-
    sportsbet_html |>
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
      map(matches, get_team_names) |> bind_rows() |> filter(!is.na(home_team)),
      map(matches, get_odds) |> bind_rows() |> filter(!is.na(home_win)),
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
    mutate(market = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market,
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
    sportsbet_html |>
    html_nodes(".linkMultiMarket_fcmecz0") |>
    html_attr("href")
  
  # Get match IDs from links
  match_ids <-
    match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()
  
  # Get data from main market page
  matches <-
    sportsbet_html |>
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
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |>
      req_headers("Referer" = "https://www.sportsbet.com.au") |>
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
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |>
      req_headers("Referer" = "https://www.sportsbet.com.au") |>
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
  
  # Anytime Touchdown-----------------------------------------------------------
  
  anytime_td <-
    top_market_data |>
    filter(str_detect(prop_market_name, "Any Time Touchdown Scorer"))
    
  # Overs
  anytime_td_overs <-
    anytime_td |>
    mutate(player_name = selection_name_prop) |>
    mutate(line = 0.5) |>
    mutate(market = "Player Touchdowns") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Combine
  anytime_td_all <-
    anytime_td_overs |>
    mutate(agency = "Sportsbet") |> 
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, agency) |> 
    distinct(match, player_name, line, over_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
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
  
  # Passing Touchdowns ---------------------------------------------------------------
  passing_touchdowns <-
    qb_prop_data |>
    filter(str_detect(prop_market_name, "\\- Passing TDs$|\\- Alt Passing TDs$"))
  
  # Overs
  passing_touchdowns_overs <-
    passing_touchdowns |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Passing Touchdowns") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  passing_touchdowns_unders <-
    passing_touchdowns |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Passing Touchdowns") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  passing_touchdowns_all <-
    passing_touchdowns_overs |>
    left_join(passing_touchdowns_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    left_join(player_teams_qb, by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  # Passing Interceptions ---------------------------------------------------------------
  passing_interceptions <-
    qb_prop_data |>
    filter(str_detect(prop_market_name, "\\- Interception$|\\- Alt Passing Ints$"))
  
  # Overs
  passing_interceptions_overs <-
    passing_interceptions |>
    filter(str_detect(selection_name_prop, "Over|Yes")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(line = 0.5) |> 
    mutate(market = "Interceptions") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  passing_interceptions_unders <-
    passing_interceptions |>
    filter(str_detect(selection_name_prop, "Under|No")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(line = 0.5) |>
    mutate(market = "Interceptions") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  passing_interceptions_all <-
    passing_interceptions_overs |>
    left_join(passing_interceptions_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    left_join(player_teams_qb, by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  # Passing Attempts ---------------------------------------------------------------
  passing_attempts <-
    qb_prop_data |>
    filter(str_detect(prop_market_name, "\\- Pass Att.*$|\\- Alt Pass Att.*$"))
  
  # Overs
  passing_attempts_overs <-
    passing_attempts |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Passing Attempts") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  passing_attempts_unders <-
    passing_attempts |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Passing Attempts") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  passing_attempts_all <-
    passing_attempts_overs |>
    left_join(passing_attempts_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    left_join(player_teams_qb, by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  # Passing Completions ---------------------------------------------------------------
  passing_completions <-
    qb_prop_data |>
    filter(str_detect(prop_market_name, "\\- Pass Completions$|\\- Alt Pass Completions$"))
  
  # Overs
  passing_completions_overs <-
    passing_completions |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Pass Completions") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  passing_completions_unders <-
    passing_completions |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Pass Completions") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  passing_completions_all <-
    passing_completions_overs |>
    left_join(passing_completions_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    mutate(player_name = ifelse(player_name == "Brian Robinson Jr.", "Brian Robinson", player_name)) |>
    mutate(player_name = ifelse(player_name == "Deebo Samuel", "Deebo Samuel Sr.", player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr), by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  #===========================================================================
  # Rushing Prop Markets
  #===========================================================================
  
  # Map function to shots urls
  rushing_prop_data <-
    map(rushing_prop_links, safe_read_prop_url)
  
  # Get just result part from output
  rushing_prop_data <-
    rushing_prop_data |>
    map("result") |>
    map_df(bind_rows)
  
  # If nrow 0 create tibble with 0 rows
  if (nrow(rushing_prop_data) == 0) {
    rushing_prop_data <-
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
  rushing_prop_data <-
    rushing_prop_data |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_prop_metadata)
  
  # Rushing Yards ---------------------------------------------------------------
  rushing_yards <-
    rushing_prop_data |>
    filter(str_detect(prop_market_name, "\\- Rushing Yds$|\\- Alt Rushing Yds$"))
  
  # Overs
  rushing_yards_overs <-
    rushing_yards |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Rushing Yards") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  rushing_yards_unders <-
    rushing_yards |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Rushing Yards") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  rushing_yards_all <-
    rushing_yards_overs |>
    left_join(rushing_yards_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    mutate(player_name = ifelse(player_name == "Brian Robinson Jr.", "Brian Robinson", player_name)) |>
    mutate(player_name = ifelse(player_name == "Deebo Samuel", "Deebo Samuel Sr.", player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr), by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  # Rushing Attempts ---------------------------------------------------------------
  rushing_attempts <-
    rushing_prop_data |>
    filter(str_detect(prop_market_name, "\\- Rush Attempts$|\\- Alt Rush Attempts$"))
  
  # Overs
  rushing_attempts_overs <-
    rushing_attempts |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Rushing Attempts") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  rushing_attempts_unders <-
    rushing_attempts |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Rushing Attempts") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  rushing_attempts_all <-
    rushing_attempts_overs |>
    left_join(rushing_attempts_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    mutate(player_name = ifelse(player_name == "Brian Robinson Jr.", "Brian Robinson", player_name)) |>
    mutate(player_name = ifelse(player_name == "Deebo Samuel", "Deebo Samuel Sr.", player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr), by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  #===========================================================================
  # Receiving Prop Markets
  #===========================================================================
  
  # Map function to shots urls
  receiving_prop_data <-
    map(receiving_prop_links, safe_read_prop_url)
  
  # Get just result part from output
  receiving_prop_data <-
    receiving_prop_data |>
    map("result") |>
    map_df(bind_rows)
  
  # If nrow 0 create tibble with 0 rows
  if (nrow(receiving_prop_data) == 0) {
    receiving_prop_data <-
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
  receiving_prop_data <-
    receiving_prop_data |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_prop_metadata)
  
  # Receiving Yards ---------------------------------------------------------------
  receiving_yards <-
    receiving_prop_data |>
    filter(str_detect(prop_market_name, "\\- Receiving Yds$|\\- Alt Receiving Yds$"))
  
  # Overs
  receiving_yards_overs <-
    receiving_yards |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Receiving Yards") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  receiving_yards_unders <-
    receiving_yards |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Receiving Yards") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  receiving_yards_all <-
    receiving_yards_overs |>
    left_join(receiving_yards_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    mutate(player_name = ifelse(player_name == "Brian Robinson Jr.", "Brian Robinson", player_name)) |>
    mutate(player_name = ifelse(player_name == "Deebo Samuel", "Deebo Samuel Sr.", player_name)) |>
    mutate(player_name = ifelse(player_name == "D.J. Moore", "DJ Moore", player_name)) |>
    mutate(player_name = ifelse(player_name == "D.K. Metcalf", "DK Metcalf", player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  # Receiving Receptions ---------------------------------------------------------------
  receiving_receptions <-
    receiving_prop_data |>
    filter(str_detect(prop_market_name, "\\- Total Receptions$|\\- Alt Receptions$"))
  
  # Overs
  receiving_receptions_overs <-
    receiving_receptions |>
    filter(str_detect(selection_name_prop, "Over|\\+")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = coalesce(as.numeric(alt_line) - 0.5, handicap)) |>
    mutate(market = "Receptions") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           over_price = prop_market_price)
  
  # Unders
  receiving_receptions_unders <-
    receiving_receptions |>
    filter(str_detect(selection_name_prop, "Under")) |>
    mutate(player_name = str_remove(prop_market_name, " \\-.*$")) |>
    mutate(alt_line = str_extract(selection_name_prop, "\\d+")) |>
    mutate(line = as.numeric(handicap)) |>
    mutate(market = "Receptions") |>
    select(match,
           home_team,
           away_team,
           player_name,
           market,
           line,
           under_price = prop_market_price)
  
  # Combine
  receiving_receptions_all <-
    receiving_receptions_overs |>
    left_join(receiving_receptions_unders) |> 
    mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
    mutate(agency = "Sportsbet") |> 
    mutate(player_name = ifelse(player_name == "Brian Robinson Jr.", "Brian Robinson", player_name)) |>
    mutate(player_name = ifelse(player_name == "Deebo Samuel", "Deebo Samuel Sr.", player_name)) |>
    mutate(player_name = ifelse(player_name == "D.J. Moore", "DJ Moore", player_name)) |>
    mutate(player_name = ifelse(player_name == "D.K. Metcalf", "DK Metcalf", player_name)) |>
    left_join(bind_rows(player_teams_qb, player_teams_rb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
    select(match, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
    distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
    arrange(match, player_name, line)
  
  #===========================================================================
  # Write all to CSV
  #===========================================================================
  
  # Anytime Touchdowns
  write_csv(anytime_td_all, "Data/scraped_odds/sportsbet_anytime_td.csv")
  
  # Passing Yards
  write_csv(passing_yards_all, "Data/scraped_odds/sportsbet_passing_yards.csv")
  
  # Passing Touchdowns
  write_csv(passing_touchdowns_all, "Data/scraped_odds/sportsbet_passing_touchdowns.csv")
  
  # Passing Interceptions
  write_csv(passing_interceptions_all, "Data/scraped_odds/sportsbet_interceptions.csv")
  
  # Passing Attempts
  write_csv(passing_attempts_all, "Data/scraped_odds/sportsbet_passing_attempts.csv")
  
  # Passing Completions
  write_csv(passing_completions_all, "Data/scraped_odds/sportsbet_passing_completions.csv")
  
  # Rushing Yards
  write_csv(rushing_yards_all, "Data/scraped_odds/sportsbet_rushing_yards.csv")
  
  # Rushing Attempts
  write_csv(rushing_attempts_all, "Data/scraped_odds/sportsbet_rushing_attempts.csv")
  
  # Receiving Yards
  write_csv(receiving_yards_all, "Data/scraped_odds/sportsbet_receiving_yards.csv")
  
  # Receiving Receptions
  write_csv(receiving_receptions_all, "Data/scraped_odds/sportsbet_receptions.csv")
  
}

# Run functions safely (align with AFL runner pattern)
safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
