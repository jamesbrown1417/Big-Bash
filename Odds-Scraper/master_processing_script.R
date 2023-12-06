#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Directory of scraped odds data
scraped_odds_dir <- "Data/scraped_odds"

# Get empirical probability function
source("Scripts/04-get-empirical-probabilities.R")

#===============================================================================
# Player Runs Data
#===============================================================================

# List all files in the directory that match player runs
player_runs_files <- list.files(scraped_odds_dir, pattern = "player_runs", full.names = TRUE)

# Read in all files
player_runs_data <- 
  player_runs_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  mutate(line = as.numeric(line)) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# Runs
distinct_run_combos <-
  player_runs_data |> 
  distinct(name = player_name, line) |> 
  mutate(stat = "runs")

player_emp_probs_2022_24 <-
  pmap(distinct_run_combos, get_empirical_prob, .progress = TRUE) |> 
  bind_rows() |> 
  select(player_name, line, games_played, empirical_prob)

# Add to main file
player_runs_data <-
  player_runs_data |> 
  left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
  rename(empirical_prob_over = empirical_prob) |> 
  mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
  mutate(diff_over = empirical_prob_over - implied_prob_over,
         diff_under = (1 - empirical_prob_under) - empirical_prob_over) |> 
  mutate(implied_prob_over = round(implied_prob_over, 2),
         empirical_prob_over = round(empirical_prob_over, 2),
         empirical_prob_under = round(empirical_prob_under, 2),
         diff_over = round(diff_over, 2),
         diff_under = round(diff_under, 2))

#===============================================================================
# Player Wickets Data
#===============================================================================

# List all files in the directory that match player wickets
player_wickets_files <- list.files(scraped_odds_dir, pattern = "player_wickets", full.names = TRUE)

# Read in all files
player_wickets_data <- 
  player_wickets_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# Wickets
distinct_run_combos <-
  player_wickets_data |> 
  distinct(name = player_name, line) |> 
  mutate(stat = "wickets")

player_emp_probs_2022_24 <-
  pmap(distinct_run_combos, get_empirical_prob, .progress = TRUE) |> 
  bind_rows() |> 
  select(player_name, line, games_played, empirical_prob)

# Add to main file
player_wickets_data <-
  player_wickets_data |> 
  left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
  rename(empirical_prob_over = empirical_prob) |> 
  mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
  mutate(diff_over = empirical_prob_over - implied_prob_over,
         diff_under = (1 - empirical_prob_under) - empirical_prob_over) |> 
  mutate(implied_prob_over = round(implied_prob_over, 2),
         empirical_prob_over = round(empirical_prob_over, 2),
         empirical_prob_under = round(empirical_prob_under, 2),
         diff_over = round(diff_over, 2),
         diff_under = round(diff_under, 2))

#===============================================================================
# Player Boundaries Data
#===============================================================================

# List all files in the directory that match player boundaries
player_boundaries_files <- list.files(scraped_odds_dir, pattern = "player_boundaries", full.names = TRUE)

# Read in all files
player_boundaries_data <- 
  player_boundaries_files |> 
  map_dfr(read_csv, col_types = cols(.default = col_character())) |> 
  arrange(player_name, line, desc(over_price)) |> 
  mutate(over_price = as.numeric(over_price),
         under_price = as.numeric(under_price)) |> 
  mutate(implied_prob_over = 1/over_price) |> 
  group_by(player_name, market, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = round((max_implied_prob - min_implied_prob), 2)) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |> 
  arrange(desc(variation), player_name, desc(over_price))

# Add empirical probabilities---------------------------------------------------

# Boundaries
distinct_run_combos <-
  player_boundaries_data |> 
  distinct(name = player_name, line) |> 
  mutate(stat = "boundaries")

player_emp_probs_2022_24 <-
  pmap(distinct_run_combos, get_empirical_prob, .progress = TRUE) |> 
  bind_rows() |> 
  select(player_name, line, games_played, empirical_prob)

# Add to main file
player_boundaries_data <-
  player_boundaries_data |> 
  left_join(player_emp_probs_2022_24, by = c("player_name", "line")) |> 
  rename(empirical_prob_over = empirical_prob) |> 
  mutate(empirical_prob_under = 1 - empirical_prob_over) |> 
  mutate(diff_over = empirical_prob_over - implied_prob_over,
         diff_under = (1 - empirical_prob_under) - empirical_prob_over) |> 
  mutate(implied_prob_over = round(implied_prob_over, 2),
         empirical_prob_over = round(empirical_prob_over, 2),
         empirical_prob_under = round(empirical_prob_under, 2),
         diff_over = round(diff_over, 2),
         diff_under = round(diff_under, 2))