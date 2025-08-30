# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)
library(nflreadr)

# Read in TAB Rushing Yards and Receiving Yards Data
tab_rushing_yards <- read_csv("Data/scraped_odds/tab_rushing_yards.csv")
tab_receiving_yards <- read_csv("Data/scraped_odds/tab_receiving_yards.csv")

# Read in BetRight Rushing Yards and Receiving Yards Data
betright_rushing_yards <- read_csv("Data/scraped_odds/betright_player_rushing_yards.csv")
betright_receiving_yards <- read_csv("Data/scraped_odds/betright_player_receiving_yards.csv")

# Select only needed variables
tab_rushing_yards <-
  tab_rushing_yards |>
  select(match, player_name, line, over_price) |> 
  filter(line %in% c(29.5, 39.5, 49.5, 59.5, 69.5, 79.5, 89.5, 99.5, 109.5, 119.5)) |> 
  mutate(line = line - 5)

tab_receiving_yards <-
  tab_receiving_yards |>
  select(match, player_name, line, over_price) |> 
  filter(line %in% c(29.5, 39.5, 49.5, 59.5, 69.5, 79.5, 89.5, 99.5, 109.5, 119.5)) |> 
  mutate(line = line - 5)

betright_rushing_yards <-
  betright_rushing_yards |>
  select(match, player_name, line, under_price)

betright_receiving_yards <-
  betright_receiving_yards |>
  select(match, player_name, line, under_price)

# Join Together
rushing_yards_arbs <-
  tab_rushing_yards |>
  left_join(betright_rushing_yards) |> 
    mutate(margin = 1 / under_price + 1 / over_price) |>
      arrange(margin) |>
      mutate(margin = (1 - margin)) |>
      mutate(margin = 100 * margin)

receiving_yards_arbs <-
  tab_receiving_yards |>
  left_join(betright_receiving_yards) |> 
    mutate(margin = 1 / under_price + 1 / over_price) |>
      arrange(margin) |>
      mutate(margin = (1 - margin)) |>
      mutate(margin = 100 * margin)