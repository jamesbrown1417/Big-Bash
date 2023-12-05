#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Directory of scraped odds data
scraped_odds_dir <- "Data/scraped_odds"

#===============================================================================
# Player Runs Data
#===============================================================================

# List all files in the directory that match player runs
player_runs_files <- list.files(scraped_odds_dir, pattern = "player_runs", full.names = TRUE)

# Read in all files
player_runs_data <- 
  player_runs_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character()))