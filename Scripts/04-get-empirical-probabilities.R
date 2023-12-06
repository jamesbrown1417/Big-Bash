#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

# Scraped Data
batting_stats_bbl <- readRDS("Data/batting_stats_bbl.rds")
bowling_stats_bbl <- readRDS("Data/bowling_stats_bbl.rds")

# Restrict to only 2021 season onwards
batting_stats_bbl <- batting_stats_bbl |> filter(start_date >= "2021-06-01")
bowling_stats_bbl <- bowling_stats_bbl |> filter(start_date >= "2021-06-01")

# Fix names
names_key <-
  readRDS("Data/player_names_linking.rds") |> 
  select(player_name_supercoach, player_name_cricinfo)

batting_stats_bbl <-
  batting_stats_bbl |> 
  left_join(names_key, by = c("player" = "player_name_cricinfo")) |> 
  rename(player_name = player_name_supercoach) |> 
  relocate(player_name, .before = player) |>
  select(-player) |> 
  filter(!is.na(player_name))

bowling_stats_bbl <-
  bowling_stats_bbl |> 
  left_join(names_key, by = c("player" = "player_name_cricinfo")) |> 
  rename(player_name = player_name_supercoach) |> 
  relocate(player_name, .before = player) |>
  select(-player) |> 
  filter(!is.na(player_name))

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(name, line, stat) {
  
  # Choose the data based on the selected player
  batting_stats_bbl <-
    batting_stats_bbl |> 
    filter(player_name == name)
  
  bowling_stats_bbl <- bowling_stats_bbl |>
    filter(player_name == name)

  # Initialize empirical_prob
  empirical_prob <- NULL
  
  # Branch based on whether stat is PTS, REB or AST
  if (stat == "runs") {
    empirical_prob <- batting_stats_bbl |> 
      summarise(games_played = n(),
                empirical_prob = mean(runs_scored >= line))
  } else if (stat == "wickets") {
    empirical_prob <- bowling_stats_bbl |> 
      summarise(games_played = n(),
                empirical_prob = mean(wickets_taken >= line))
  } else if (stat == "fours") {
    empirical_prob <- batting_stats_bbl |> 
      summarise(games_played = n(),
                empirical_prob = mean(fours >= line))
  } else if (stat == "sixes") {
    empirical_prob <- batting_stats_bbl |> 
      summarise(games_played = n(),
                empirical_prob = mean(sixes >= line))
  } else {
    stop("stat must be one of runs, wickets, fours, sixes")
  }
  
  # Add line, player_name, and season information
  empirical_prob <- empirical_prob |> 
    mutate(line = line, 
           player_name = name)
  
  # Rename the empirical_prob column to include season
  new_col_name <- paste("empirical_prob")
  empirical_prob <- empirical_prob |> 
    rename_with(~ new_col_name, .cols = "empirical_prob")
  
  # Return empirical probability
  return(empirical_prob)
}

get_empirical_prob(name = "Aaron Finch", line = 9.5, stat = "runs")
