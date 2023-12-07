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

# Get Batting Position
batting_position_bbl_strikers <-
  ball_by_ball_data_bbl |>
  select(match_id, innings, striker, non_striker, bowler, over, ball, wickets_lost_yet) |>
  group_by(match_id, innings, striker) |>
  slice_head(n = 1) |>
  mutate(player = striker, role = "Striker") |> 
  ungroup() |> 
  select(-striker, -non_striker)

batting_position_bbl_non_strikers <-
  ball_by_ball_data_bbl |>
  select(match_id, innings, striker, non_striker, bowler, over, ball, wickets_lost_yet) |>
  group_by(match_id, innings, non_striker) |>
  slice_head(n = 1) |>
  mutate(player = non_striker, role = "Non-Striker") |> 
  ungroup() |> 
  select(-striker, -non_striker)

batting_position_bbl <-
  bind_rows(batting_position_bbl_strikers, batting_position_bbl_non_strikers) |>
  arrange(match_id, innings, over, ball, player, desc(role)) |>
  group_by(match_id, innings, player) |>
  slice_head(n = 1) |>
  arrange(match_id, innings, over, ball, player, desc(role)) |>
  group_by(match_id, innings) |> 
  mutate(batting_position = row_number()) |>
  select(match_id, innings, player, batting_position)
  
batting_stats_bbl <-
  batting_stats_bbl |> 
  left_join(batting_position_bbl, by = c("match_id", "innings", "player"))

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

# Standardise Venue Names
batting_stats_bbl <-
  batting_stats_bbl |>
  mutate(venue = case_when(
    str_detect(venue, "Brisbane Cricket Ground") ~ "The Gabba",
    str_detect(venue, "Sydney Cricket Ground") ~ "SCG",
    str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
    str_detect(venue, "Manuka Oval") ~ "Manuka Oval",
    str_detect(venue, "Melbourne Cricket Ground") ~ "MCG",
    str_detect(venue, "Perth Stadium") ~ "Perth Stadium",
    str_detect(venue, "Blundstone Arena") ~ "Blundstone Arena",
    str_detect(venue, "Docklands") ~ "Marvel Stadium",
    str_detect(venue, "Marvel Stadium") ~ "Marvel Stadium",
    str_detect(venue, "International Sports Stadium") ~ "Coffs Harbour",
    str_detect(venue, "Sydney Showground Stadium") ~ "Sydney Showground Stadium",
    str_detect(venue, "Carrara Oval") ~ "Carrara Oval",
    str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
    str_detect(venue, "Traeger Park") ~ "Traeger Park",
    str_detect(venue, "Metricon Stadium") ~ "Metricon Stadium",
    str_detect(venue, "University of Tasmania Stadium") ~ "University of Tasmania Stadium",
    str_detect(venue, "(UTAS)|(Aurora)") ~ "University of Tasmania Stadium",
    str_detect(venue, "Bellerive") ~ "Bellerive Oval",
    str_detect(venue, "Simonds|Kardinia|GMHBA") ~ "GMHBA Stadium",
    str_detect(venue, "Western Australia Cricket Association Ground|W\\.A\\.C\\.A") ~ "WACA",
    TRUE ~ venue))

bowling_stats_bbl <-
  bowling_stats_bbl |>
  mutate(venue = case_when(
    str_detect(venue, "Brisbane Cricket Ground") ~ "The Gabba",
    str_detect(venue, "Sydney Cricket Ground") ~ "SCG",
    str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
    str_detect(venue, "Manuka Oval") ~ "Manuka Oval",
    str_detect(venue, "Melbourne Cricket Ground") ~ "MCG",
    str_detect(venue, "Perth Stadium") ~ "Perth Stadium",
    str_detect(venue, "Blundstone Arena") ~ "Blundstone Arena",
    str_detect(venue, "Docklands") ~ "Marvel Stadium",
    str_detect(venue, "Marvel Stadium") ~ "Marvel Stadium",
    str_detect(venue, "International Sports Stadium") ~ "Coffs Harbour",
    str_detect(venue, "Sydney Showground Stadium") ~ "Sydney Showground Stadium",
    str_detect(venue, "Carrara Oval") ~ "Carrara Oval",
    str_detect(venue, "Adelaide Oval") ~ "Adelaide Oval",
    str_detect(venue, "Traeger Park") ~ "Traeger Park",
    str_detect(venue, "Metricon Stadium") ~ "Metricon Stadium",
    str_detect(venue, "University of Tasmania Stadium") ~ "University of Tasmania Stadium",
    str_detect(venue, "(UTAS)|(Aurora)") ~ "University of Tasmania Stadium",
    str_detect(venue, "Bellerive") ~ "Bellerive Oval",
    str_detect(venue, "Simonds|Kardinia|GMHBA") ~ "GMHBA Stadium",
    str_detect(venue, "Western Australia Cricket Association Ground|W\\.A\\.C\\.A") ~ "WACA",
    TRUE ~ venue))

# Write out data
write_rds(bowling_stats_bbl, "Data/bowling_stats_bbl.rds")
write_rds(batting_stats_bbl, "Data/batting_stats_bbl.rds")

# Write out player names by season dataset
player_data_bbl |> 
  left_join(match_data_bbl[, c("match_id", "season")]) |> 
  distinct(team, player, season) |> 
  arrange(player, desc(season)) |> 
  write_rds("Data/player_names_teams_bbl.rds")
