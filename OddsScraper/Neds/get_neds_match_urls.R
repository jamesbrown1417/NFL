# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Read in json
json_text <- readLines("OddsScraper/EPL/Neds/neds_response.json", encoding = "UTF-8")
clean_json <- iconv(json_text, "UTF-8", "ASCII", sub = "")
neds_response <- jsonlite::fromJSON(clean_json)

# Initialize empty lists to store data
event_name <- character()
event_id <- character()
competition_name <- character()

# Extract event IDs and names from JSON response
for (value in neds_response$events) {
  event_name <- c(event_name, value$name)
  event_id <- c(event_id, value$id)
  competition_name <- c(competition_name, value$competition$name)
}

# Create a data frame from the vectors
df <- data.frame(event_name, event_id, competition_name)

# Filter the data frame to only include matches with ' vs ' in the event name
df <- df |> filter(str_detect(event_name, ' vs '))

# Only get EPL Games
df <- df |> filter(str_detect(competition_name, '^Premier League$'))

df$url <-
  paste0("https://www.neds.com.au/sports/soccer/uk-ireland/premier-league/",
         tolower(gsub(" ", "-", df$event_name)),
         "/",
         df$event_id)

# Write out as csv
write_csv(df, "OddsScraper/EPL/Neds/neds_epl_match_urls.csv")
