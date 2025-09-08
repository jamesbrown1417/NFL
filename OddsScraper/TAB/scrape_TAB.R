# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(httr2)
library(jsonlite)
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


# Get response body
tab_response <- fromJSON("OddsScraper/TAB/tab_response.json")

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
  
  # Market info
  markets_name = markets$betOption
  market_propositions = markets$propositions
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$name
  match_start_time = matches$startTime
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions
  )
}

# List of matches
matches <- map(1:nrow(tab_response$matches), ~ tab_response$matches[., ])

# Map functions to data
all_tab_markets <-
  map(matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest(cols = c(propositions)) |> 
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_id = id,
         prop_name = name,
         price = returnWin)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_tab_markets |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  filter(market_name == "Head To Head") |>
  group_by(match) |>
  filter(row_number() == 1) |>
  rename(home_win = price) |>
  select(-prop_name, -prop_id)

# Away teams
away_teams <-
  all_tab_markets |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  filter(market_name == "Head To Head") |>
  group_by(match) |>
  filter(row_number() == 2) |>
  rename(away_win = price) |>
  select(-prop_name, -prop_id)

# Combine
tab_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |>
  select(match,
         start_time,
         market = market_name,
         home_team,
         home_win,
         away_team,
         away_win) |>
  mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
  mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
  tab_head_to_head_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(market = "Head To Head")

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")

#===============================================================================
# Total Points
#===============================================================================

# Total Points Overs
total_points_overs <-
  all_tab_markets |>
  filter(market_name == "Total Points Over/Under" |
           market_name == "Pick Your Own Total") |>
  filter(str_detect(prop_name, "Over")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.\\d+")) |>
  select(match, start_time, line, price) |>
  rename(over_price = price)

# Total Points Unders
total_points_unders <-
  all_tab_markets |>
  filter(market_name == "Total Points Over/Under" |
           market_name == "Pick Your Own Total") |>
  filter(str_detect(prop_name, "Under")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.\\d+")) |>
  select(match, start_time, line, price) |>
  rename(under_price = price)

# Combine
tab_total_points_markets <-
  total_points_overs |>
  left_join(total_points_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Total Points") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  select(match, start_time, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, line)

#===============================================================================
# Touchdowns
#===============================================================================

# Touchdowns Overs
touchdown_overs <-
  all_tab_markets |>
  filter(str_detect(market_name, "To Score .* Touchdown.*$")) |> 
  mutate(market_name = ifelse(market_name == "To Score a Touchdown", "To Score 1+ Touchdowns", market_name)) |>
  mutate(line = str_extract(market_name, "\\d+\\.?\\d?")) |>
  # If line doesnt end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " \\(.*$")) |> 
  separate(player_name, into = c("last_name", "first_name"), sep = " ") |> 
  mutate(last_name = str_to_title(last_name)) |>
  mutate(first_name = str_to_title(first_name)) |>
  mutate(player_name = paste(first_name, last_name, sep = " ")) |> 
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price) |> 
  mutate(agency = "TAB") |> 
  mutate(market = "Player Touchdowns") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_rb, player_teams_qb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, agency) |> 
  distinct(match, player_name, line, over_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(touchdown_overs, "Data/scraped_odds/tab_touchdowns.csv")

#===============================================================================
# Passing Yards
#===============================================================================

# Passing Yards Overs
passing_yards_overs <-
  all_tab_markets |>
  filter(market_name == "Passing Yards O/U" |
         market_name == "Alternate Passing Yards") |>
  filter(str_detect(prop_name, "Ovr|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  # If line doesnt end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Ovr.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Passing Yards Unders
passing_yards_unders <-
  all_tab_markets |>
  filter(market_name == "Passing Yards O/U" |
         market_name == "Alternate Passing Yards") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Combine
tab_passing_yards_markets <-
  passing_yards_overs |>
  left_join(passing_yards_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Passing Yards") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb, by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_passing_yards_markets, "Data/scraped_odds/tab_passing_yards.csv")

#===============================================================================
# Passing Attempts
#===============================================================================

# Passing Attempts Overs
passing_attempts_overs <-
  all_tab_markets |>
  filter(market_name == "Passing Attempts O/U" |
           market_name == "Alternate Passing Attempts") |>
  filter(str_detect(prop_name, "Ovr|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Ovr.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Passing Attempts Unders
passing_attempts_unders <-
  all_tab_markets |>
  filter(market_name == "Passing Attempts O/U" |
           market_name == "Alternate Passing Attempts") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Combine
tab_passing_attempts_markets <-
  passing_attempts_overs |>
  left_join(passing_attempts_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Passing Attempts") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb, by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_passing_attempts_markets, "Data/scraped_odds/tab_passing_attempts.csv")

#===============================================================================
# Interceptions
#===============================================================================

# Interceptions Overs
interceptions_overs <-
  all_tab_markets |>
  filter(market_name == "Interceptions O/U" |
           market_name == "Alternate Interceptions") |>
  filter(str_detect(prop_name, "Over|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Over.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Interceptions Unders
interceptions_unders <-
  all_tab_markets |>
  filter(market_name == "Interceptions O/U" |
           market_name == "Alternate Interceptions") |>
  filter(str_detect(prop_name, "Under|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Under.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Combine
tab_interceptions_markets <-
  interceptions_overs |>
  left_join(interceptions_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Interceptions") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb, by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_interceptions_markets, "Data/scraped_odds/tab_interceptions.csv")

#===============================================================================
# Passing Touchdowns
#===============================================================================

# Passing Touchdowns Overs
passing_touchdowns_overs <-
  all_tab_markets |>
  filter(market_name == "Passing Touchdowns O/U" |
           market_name == "QB Passing Touchdowns") |>
  filter(str_detect(prop_name, "Ove|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Over.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Passing Touchdowns Unders
passing_touchdowns_unders <-
  all_tab_markets |>
  filter(market_name == "Passing Touchdowns O/U") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Combine
tab_passing_touchdowns_markets <-
  passing_touchdowns_overs |>
  left_join(passing_touchdowns_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Passing Touchdowns") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_teams_qb, by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_passing_touchdowns_markets, "Data/scraped_odds/tab_passing_touchdowns.csv")

#===============================================================================
# Rushing Yards
#===============================================================================

# Rushing Yards Overs
rushing_yards_overs <-
  all_tab_markets |>
  filter(market_name == "Rushing Yards O/U" |
           market_name == "Alternate Rushing Yards") |>
  filter(str_detect(prop_name, "Ov|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Ov.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Rushing Yards Unders
rushing_yards_unders <-
  all_tab_markets |>
  filter(market_name == "Rushing Yards O/U" |
           market_name == "Alternate Rushing Yards") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d+")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Alternate Rushing Yards
rushing_yards_alternate <-
  all_tab_markets |>
  filter(str_detect(market_name,"Rushing Yards$")) |>
  mutate(line = str_extract(market_name, "\\d+\\.?\\d+")) |>
  mutate(line = as.numeric(line)-0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Combine Overs with Alt Lines
rushing_yards_overs <-
  rushing_yards_overs |> 
  bind_rows(rushing_yards_alternate)

# Combine
tab_rushing_yards_markets <-
  rushing_yards_overs |>
  left_join(rushing_yards_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Rushing Yards") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_rb, player_teams_qb, player_teams_wr), by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_rushing_yards_markets, "Data/scraped_odds/tab_rushing_yards.csv")

#===============================================================================
# Rushing Attempts
#===============================================================================

# Rushing Attempts Overs
rushing_attempts_overs <-
  all_tab_markets |>
  filter(market_name == "Rushing Attempts O/U" |
           market_name == "Alternate Rushing Attempts") |>
  filter(str_detect(prop_name, "Ovr|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Ovr.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Rushing Attempts Unders
rushing_attempts_unders <-
  all_tab_markets |>
  filter(market_name == "Rushing Attempts O/U" |
           market_name == "Alternate Rushing Attempts") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Combine
tab_rushing_attempts_markets <-
  rushing_attempts_overs |>
  left_join(rushing_attempts_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Rushing Attempts") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_rb, player_teams_qb, player_teams_wr), by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_rushing_attempts_markets, "Data/scraped_odds/tab_rushing_attempts.csv")

#===============================================================================
# Receiving Yards
#===============================================================================

# Receiving Yards Overs
receiving_yards_overs <-
  all_tab_markets |>
  filter(market_name == "Receiving Yards O/U" |
           market_name == "Alternate Receiving Yards") |>
  filter(str_detect(prop_name, "Over|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Over.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Receiving Yards Unders
receiving_yards_unders <-
  all_tab_markets |>
  filter(market_name == "Receiving Yards O/U" |
           market_name == "Alternate Receiving Yards") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Alternate Receiving Yards
receiving_yards_alternate <-
  all_tab_markets |>
  filter(str_detect(market_name,"^\\d+.*Receiving Yards$")) |>
  mutate(line = str_extract(market_name, "\\d+\\.?\\d+")) |>
  mutate(line = as.numeric(line)-0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Combine Overs with Alt Lines
receiving_yards_overs <-
  receiving_yards_overs |> 
  bind_rows(receiving_yards_alternate)

# Combine
tab_receiving_yards_markets <-
  receiving_yards_overs |>
  left_join(receiving_yards_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Receiving Yards") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_rb, player_teams_qb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_receiving_yards_markets, "Data/scraped_odds/tab_receiving_yards.csv")

#===============================================================================
# Receptions
#===============================================================================

# Receptions Overs
receptions_overs <-
  all_tab_markets |>
  filter(market_name == "Receptions O/U" |
           market_name == "Alternate Receptions") |>
  filter(str_detect(prop_name, "Ovr|\\+")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  # If line doesn't end in .5, minus 0.5
  mutate(line = if_else(str_detect(line, "\\."), as.numeric(line), as.numeric(line) - 0.5)) |>
  mutate(player_name = str_remove(prop_name, " Ovr.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(over_price = price)

# Receptions Unders
receptions_unders <-
  all_tab_markets |>
  filter(market_name == "Receptions O/U" |
           market_name == "Alternate Receptions") |>
  filter(str_detect(prop_name, "Und|\\-")) |>
  mutate(line = str_extract(prop_name, "\\d+\\.?\\d?")) |>
  mutate(line = as.numeric(line)) |>
  mutate(player_name = str_remove(prop_name, " Und.*$")) |>
  mutate(player_name = str_remove(player_name, " \\d+.*$")) |>
  select(match, start_time, player_name, line, price) |>
  rename(under_price = price)

# Combine
tab_receptions_markets <-
  receptions_overs |>
  left_join(receptions_unders, relationship = "many-to-many") |>
  mutate(margin = round((1 / over_price + 1 / under_price), digits = 3)) |>
  mutate(agency = "TAB") |> 
  mutate(market = "Receptions") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(bind_rows(player_teams_rb, player_teams_qb, player_teams_wr, player_teams_te, player_teams_db), by = "player_name") |>
  select(match, start_time, player_name, player_team, market, line, over_price, under_price, margin, agency) |> 
  distinct(match, player_name, line, over_price, under_price, .keep_all = TRUE) |> 
  arrange(start_time, match, player_name, line)

# Write to csv
write_csv(tab_receptions_markets, "Data/scraped_odds/tab_receptions.csv")
