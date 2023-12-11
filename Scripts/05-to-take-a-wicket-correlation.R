library(tidyverse)

# Scraped Data
batting_stats_bbl <- readRDS("Data/batting_stats_bbl.rds")
bowling_stats_bbl <- readRDS("Data/bowling_stats_bbl.rds")

# Improved function to calculate probability and correlation of two players taking at least one wicket
calculate_wicket_probabilities <- function(bowling_data, player_pattern_1, player_pattern_2, innings_nums = c(1, 2)) {
  # Parameter validation
  if (!is.data.frame(bowling_data)) {
    stop("bowling_data must be a data frame")
  }
  if (!is.character(player_pattern_1) | !is.character(player_pattern_2)) {
    stop("Player patterns must be character strings")
  }
  if (!all(innings_nums %in% 1:2)) {
    stop("innings_nums must be 1 or 2")
  }
  
  # Filter and process data
  filtered_data <- bowling_data %>%
    filter(innings %in% innings_nums, balls_bowled >= 24) %>%
    filter(str_detect(player, player_pattern_1) | str_detect(player, player_pattern_2)) %>%
    group_by(match_id, team, start_date) %>%
    mutate(
      at_least_1_wicket_a = ifelse(str_detect(player, player_pattern_1), wickets_taken >= 1, NA),
      at_least_1_wicket_b = ifelse(str_detect(player, player_pattern_2), wickets_taken >= 1, NA)
    ) %>%
    summarise(
      both_take_1_wicket = any(at_least_1_wicket_a, na.rm = TRUE) & any(at_least_1_wicket_b, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate probability
  prob_both_take_1_wicket <- mean(filtered_data$both_take_1_wicket, na.rm = TRUE)
  
  # Calculate implied odds
  implied_odds <- 1 / prob_both_take_1_wicket
  
  # Games Played
  games_played <- nrow(filtered_data)
  
  # Calculate Correlation (if feasible)
  correlation <- if (games_played > 0) {
    cor(
      filtered_data$at_least_1_wicket_a,
      filtered_data$at_least_1_wicket_b,
      use = "complete.obs",
      method = "pearson"
    )
  } else {
    NA
  }
  
  # Return a list
  list(
    player_1 = player_pattern_1,
    player_2 = player_pattern_2,
    correlation = round(correlation, 3),
    implied_odds = round(implied_odds, 2),
    sample_size = games_played
  )
}

# Example usage
# calculate_wicket_probabilities(bowling_stats_bbl, "Player1", "Player2", innings_nums = c(1, 2))
