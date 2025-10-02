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
fixture_urls <- paste0("https://api.dabble.com.au/sportfixtures/details/", fixtures$id)
fixture_urls_dfs <- paste0("https://api.dabble.com.au/sportfixtures/details/", fixtures$id, "?filter=dfs-enabled")

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
# Get QB Passing Yards
#===============================================================================

# Filter to player passing yards markets
player_passing_yards_markets <-
  prop_data |> 
  filter(str_detect(selection_name, "Passing Yards") | str_detect(prop_name, "Passing Yards")) |> 
  filter(str_detect(selection_name, "Rushing|Receiving|Half", negate = TRUE)) |>
  filter(str_detect(prop_name, "Rushing|Receiving|Half", negate = TRUE)) |>
  mutate(price = round(price*0.9, digits = 2))

# Over lines
over_lines <-
  player_passing_yards_markets |> 
  filter(str_detect(selection_name, "Over")) |> 
  mutate(market_name = "Player Passing Yards") |>
  mutate(player_name = str_remove(selection_name, " Over .*")) |>
  mutate(line = as.numeric(str_extract(selection_name, "\\d+\\.\\d+"))) |>
  select(match, market_name, player_name, line, over_price = price)

# Alt lines
alt_lines <-
  player_passing_yards_markets |> 
  filter(str_detect(prop_name, "\\+")) |> 
  mutate(market_name = "Player Passing Yards") |>
  mutate(player_name = selection_name) |> 
  mutate(line = as.numeric(str_extract(prop_name, "\\d+")) - 0.5) |>
  select(match, market_name, player_name, line, over_price = price)

# Under lines
under_lines <-
  player_passing_yards_markets |> 
  filter(str_detect(selection_name, "Under")) |> 
  mutate(market_name = "Player Passing Yards") |>
  mutate(player_name = str_remove(selection_name, " Under .*")) |>
  mutate(line = as.numeric(str_extract(selection_name, "\\d+\\.\\d+"))) |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
player_passing_yards <-
  over_lines |>
  bind_rows(alt_lines) |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Fix team and player names-----------------------------------------------------
#===============================================================================

# Fix player names--------------------------------------------------------------

# Apply player name cleaning to all markets
player_passing_yards <- player_passing_yards |> mutate(player_name = fix_player_names(player_name))

# Fix Team Names----------------------------------------------------------------

# Apply team name fixes to all markets
player_passing_yards <- player_passing_yards |> 
  separate(match, c("away_team", "home_team"), sep = " @ ") |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))


#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

player_passing_yards |> write_csv("Data/scraped_odds/dabble_player_passing_yards.csv")

