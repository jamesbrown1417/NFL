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

# Read in sportsbet rushing yards and receiving yards data
sportsbet_rushing_yards <- read_csv("Data/scraped_odds/sportsbet_rushing_yards.csv")
sportsbet_receiving_yards <- read_csv("Data/scraped_odds/sportsbet_receiving_yards.csv")

# Select only needed variables
tab_rushing_yards <-
  tab_rushing_yards |>
  select(match, player_name, over_line = line, over_price, over_agency = agency) |> 
  filter((over_line + 0.5) %% 5 == 0) |>
  mutate(over_line = over_line - 5)

tab_receiving_yards <-
  tab_receiving_yards |>
  select(match, player_name, over_line = line, over_price, over_agency = agency) |> 
  filter((over_line + 0.5) %% 5 == 0) |>
  mutate(over_line = over_line - 5)

other_rushing_yards <-
  betright_rushing_yards |>
  bind_rows(sportsbet_rushing_yards) |>
  filter(!is.na(under_price)) |> 
  select(match, player_name, under_line = line, under_price, under_agency = agency)

other_receiving_yards <-
  betright_receiving_yards |>
  bind_rows(sportsbet_receiving_yards) |>
  filter(!is.na(under_price)) |>
  select(match, player_name, under_line = line, under_price, under_agency = agency)

# Join Together
rushing_yards_arbs <-
  tab_rushing_yards |>
  left_join(other_rushing_yards, relationship = "many-to-many") |> 
  filter(over_line <= under_line) |>
    mutate(margin = 1 / under_price + 1 / over_price) |>
      arrange(margin) |>
      mutate(margin = (1 - margin)) |>
      mutate(margin = 100 * margin) |> 
  filter(over_line >= 29.5)

receiving_yards_arbs <-
  tab_receiving_yards |>
  left_join(other_receiving_yards, relationship = "many-to-many") |> 
  filter(over_line <= under_line) |>
    mutate(margin = 1 / under_price + 1 / over_price) |>
      arrange(margin) |>
      mutate(margin = (1 - margin)) |>
      mutate(margin = 100 * margin) |> 
  filter(over_line >= 29.5)
