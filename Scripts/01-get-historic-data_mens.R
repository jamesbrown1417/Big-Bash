##%######################################################%##
#                                                          #
####              Libraries and functions               ####
#                                                          #
##%######################################################%##

library(tidyverse)
library(cricketdata)

##%######################################################%##
#                                                          #
####              Get data from cricsheet               ####
#                                                          #
##%######################################################%##

# Get match level dataset
match_data_bbl <-
  fetch_cricsheet(type = "match",
                  gender = "male",
                  competition = "bbl")

# Get player level dataset
player_data_bbl <-
  fetch_cricsheet(type = "player",
                  gender = "male",
                  competition = "bbl")

# Get ball by ball level dataset
ball_by_ball_data_bbl <-
  fetch_cricsheet(type = "bbb",
                  gender = "male",
                  competition = "bbl") |> 
  mutate(match_id = as.character(match_id))

##%######################################################%##
#                                                          #
####  Aggregate ball by ball data to get player stats   ####
#                                                          #
##%######################################################%##

# Batting stats
batting_stats_bbl <-
ball_by_ball_data_bbl |>
  group_by(striker, match_id, start_date, venue) |>
  summarise(
    innings = min(innings),
    balls_faced = n(),
    runs_scored = sum(runs_off_bat, na.rm = TRUE),
    fours = sum(runs_off_bat == 4, na.rm = TRUE),
    sixes = sum(runs_off_bat == 6, na.rm = TRUE),
    not_out = max(wicket, na.rm = TRUE) == 0
  ) |> 
  left_join(player_data_bbl, by = c("striker" = "player", "match_id" = "match_id")) |>
  ungroup() |> 
  arrange(desc(runs_scored)) |> 
  rename(player = striker)

# Bowling stats
bowling_stats_bbl <-
  ball_by_ball_data_bbl |> 
  group_by(bowler, match_id, start_date, venue) |>
  summarise(
    innings = min(innings),
    balls_bowled = n(),
    runs_conceded = sum(runs_off_bat + extras, na.rm = TRUE),
    wickets_taken = sum(wicket_type != "" & wicket_type != "run out", na.rm = TRUE),
    economy_rate = runs_conceded / (balls_bowled / 6)
  ) |> 
  left_join(player_data_bbl, by = c("bowler" = "player", "match_id" = "match_id")) |>
  ungroup() |> 
  arrange(desc(wickets_taken)) |> 
  rename(player = bowler)

##%######################################################%##
#                                                          #
####                  Write out as RDS                  ####
#                                                          #
##%######################################################%##

# Write out data
write_rds(bowling_stats_bbl, "Data/bowling_stats_bbl.rds")
write_rds(batting_stats_bbl, "Data/batting_stats_bbl.rds")

# Write out player names by season dataset
player_data_bbl |> 
  left_join(match_data_bbl[, c("match_id", "season")]) |> 
  distinct(team, player, season) |> 
  arrange(player, desc(season)) |> 
  write_rds("Data/player_names_teams_bbl.rds")
