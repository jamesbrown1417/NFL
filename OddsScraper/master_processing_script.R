#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name, echo = FALSE)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
run_scraping("OddsScraper/scrape_TAB.R")
run_scraping("OddsScraper/scrape_Sportsbet.R")
run_scraping("OddsScraper/scrape_pointsbet.R")
run_scraping("OddsScraper/Neds/scrape_neds.R")
run_scraping("OddsScraper/Pinnacle/tidy_pinnacle.R")

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <-
  map(list_of_h2h_files, read_csv)

# Combine
h2h_data <-
  list_of_h2h_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(-start_time) |> 
  select(match:agency)

# Write out
write_rds(h2h_data, "Data/processed_odds/h2h_data.rds")

#===============================================================================
# Passing Yards
#===============================================================================

# Read in all passing yards data
list_of_passing_yards_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "passing_yards")

# Read in all passing yards data
list_of_passing_yards_data <-
  map(list_of_passing_yards_files, read_csv)

# Combine
passing_yards_data <-
  list_of_passing_yards_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  select(-home_team, -away_team, -opposition_team)

# Write out
write_rds(passing_yards_data, "Data/processed_odds/passing_yards_data.rds")

#===============================================================================
# Passing Touchdowns
#===============================================================================

# Read in all passing touchdowns data
list_of_passing_td_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "passing_touchdowns|passing_td")

# Read in all passing touchdowns data
list_of_passing_td_data <-
  map(list_of_passing_td_files, read_csv)

# Combine
passing_td_data <-
  list_of_passing_td_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  relocate(under_price, .after = over_price) |>
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  select(-home_team, -away_team, -opposition_team)

# Write out
write_rds(passing_td_data, "Data/processed_odds/passing_td_data.rds")

#===============================================================================
# Passing Attempts
#===============================================================================

# Read in all passing attempts data
list_of_passing_attempts_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "passing_attempts")

# Read in all passing attempts data
list_of_passing_attempts_data <-
  map(list_of_passing_attempts_files, read_csv)

# Combine
passing_attempts_data <-
  list_of_passing_attempts_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  select(-home_team, -away_team, -opposition_team)


# Write out
write_rds(passing_attempts_data, "Data/processed_odds/passing_attempts_data.rds")

#===============================================================================
# Passing Completions
#===============================================================================

# Read in all passing completions data
list_of_passing_completions_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "passing_completions")

# Read in all passing completions data
list_of_passing_completions_data <-
  map(list_of_passing_completions_files, read_csv)

# Combine
passing_completions_data <-
  list_of_passing_completions_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
write_rds(passing_completions_data, "Data/processed_odds/passing_completions_data.rds")

#===============================================================================
# Interceptions
#===============================================================================

# Read in all interceptions data
list_of_interceptions_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "interceptions")

# Read in all interceptions data
list_of_interceptions_data <-
  map(list_of_interceptions_files, read_csv)

# Combine
interceptions_data <-
  list_of_interceptions_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
write_rds(interceptions_data, "Data/processed_odds/interceptions_data.rds")

#===============================================================================
# Rushing Yards
#===============================================================================

# Read in all rushing yards data
list_of_rushing_yards_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "rushing_yards")

# Read in all rushing yards data
list_of_rushing_yards_data <-
  map(list_of_rushing_yards_files, read_csv)

# Combine
rushing_yards_data <-
  list_of_rushing_yards_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  select(-home_team, -away_team, -opposition_team)

# Write out
write_rds(rushing_yards_data, "Data/processed_odds/rushing_yards_data.rds")

#===============================================================================
# Rushing Attempts
#===============================================================================

# Read in all rushing attempts data
list_of_rushing_attempts_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "rushing_attempts")

# Read in all rushing attempts data
list_of_rushing_attempts_data <-
  map(list_of_rushing_attempts_files, read_csv)

# Combine
rushing_attempts_data <-
  list_of_rushing_attempts_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
write_rds(rushing_attempts_data, "Data/processed_odds/rushing_attempts_data.rds")

#===============================================================================
# Receiving Yards
#===============================================================================

# Read in all receiving yards data
list_of_receiving_yards_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "receiving_yards")

# Read in all receiving yards data
list_of_receiving_yards_data <-
  map(list_of_receiving_yards_files, read_csv)

# Combine
receiving_yards_data <-
  list_of_receiving_yards_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  select(-home_team, -away_team, -opposition_team)

# Write out
write_rds(receiving_yards_data, "Data/processed_odds/receiving_yards_data.rds")

#===============================================================================
# Receptions
#===============================================================================

# Read in all receptions data
list_of_receptions_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "receptions")

# Read in all receptions data
list_of_receptions_data <-
  map(list_of_receptions_files, read_csv)

# Combine
receptions_data <-
  list_of_receptions_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(match:agency) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  select(-home_team, -away_team, -opposition_team)

# Write out
write_rds(receptions_data, "Data/processed_odds/receptions_data.rds")

#===============================================================================
# Touchdowns
#===============================================================================

# Read in all touchdowns data
list_of_touchdowns_files <- list.files("Data/scraped_odds/", full.names = TRUE, pattern = "touchdowns")

# Read in all touchdowns data
list_of_touchdowns_data <-
  map(list_of_touchdowns_files, read_csv)

# Combine
touchdowns_data <-
  list_of_touchdowns_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  filter(market != "Passing Touchdowns") |>
  arrange(match) |> 
  select(match:agency, under_price) |> 
  arrange(match, player_name, line, desc(over_price)) |> 
  relocate(under_price, .after = over_price) |>
  select(-home_team, -away_team, -opposition_team)

# Write out
write_rds(touchdowns_data, "Data/processed_odds/player_touchdowns_data.rds")
