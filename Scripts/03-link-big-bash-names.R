#===============================================================================
# Libraries and functions
#===============================================================================

library(cricketdata)
library(tidyverse)

#===============================================================================
# Read in Data
#===============================================================================

# Scraped Data
batting_stats_bbl <- readRDS("Data/batting_stats_bbl.rds")
bowling_stats_bbl <- readRDS("Data/bowling_stats_bbl.rds")

# Supercoach names
supercoach_data <-
  read_csv("Data/supercoach-data.csv") |> 
  mutate(join_name = paste(substr(player_first_name, 1, 1), player_last_name, sep = " ")) |>
  select(player_name_supercoach = player_name, join_name)

player_names <-
  bind_rows(batting_stats_bbl, bowling_stats_bbl) |> 
  distinct(player) |> 
  separate(player, c("player_first_name", "player_last_name_1", "player_last_name_2"), sep = " ", remove = FALSE) |>
  mutate(player_last_name_2 = ifelse(is.na(player_last_name_2), "", player_last_name_2)) |>
  mutate(join_name = paste(substr(player_first_name, 1, 1), player_last_name_1, player_last_name_2, sep = " ")) |> 
  mutate(join_name = str_remove(join_name, " $")) |>
  select(join_name, player_name_cricinfo = player)

#===============================================================================
# Attempt to join names
#===============================================================================

all_names <-
supercoach_data |> 
  left_join(player_names, by = "join_name")

# Write out as rds
all_names |> 
  write_rds("Data/player_names_linking.rds")