# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Get fixture and player team data
player_teams <- read_csv("Data/supercoach-data.csv")
fixture <- read_csv("Data/supercoach-fixture.csv")

# Get each teams next match
home_matches <-
  fixture |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(team = home_team, match, start_date) |>
  mutate(start_date = start_date + hours(10) + minutes(30)) |>  
  filter(start_date >= today())

away_matches <-
  fixture |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(team = away_team, match, start_date) |>
  mutate(start_date = start_date + hours(10) + minutes(30)) |>  
  filter(start_date >= today())

# Combine together
all_matches <-
  bind_rows(home_matches, away_matches) |>
  arrange(team, start_date) |> 
  group_by(team) |> 
  slice_head(n = 1) |> 
  ungroup()

# Fix certain problematic player names
player_teams <-
  player_teams |>
  mutate(
    player_name = if_else(
      player_name == "Tom Rogers" &
        player_team == "Melbourne Stars",
      "Tom F Rogers",
      player_name
    )
  )

# Make initial variable
player_teams_initials <-
  player_teams |> 
  mutate(player_name_full = player_name) |> 
  mutate(player_name = paste(str_sub(player_first_name, 1, 1), player_last_name)) |> 
  mutate(
    player_name = if_else(
      player_name == "T Rogers" &
        player_team == "Melbourne Stars",
      "T F Rogers",
      player_name
    )
  )

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/Cricket/featured?homeState=SA&jurisdiction=SA"

# Make request and get response
tab_response <-
  request(tab_url) |>
  req_perform() |> 
  resp_body_json()

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
  
  # Market info
  markets_name = markets$betOption
  market_propositions = markets$propositions
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$name
  match_round = matches$round
  match_start_time = matches$startTime
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    round = match_round,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions
  )
}

# Get competitions
tab_competitions <-
  tab_response$competitions |> 
  map(~ .x$name)

# Get element of competitions that equals "Big Bash"
bbl_index <- which(tab_competitions == "Big Bash")

# Map functions to data
all_tab_markets <-
  map(tab_response$competitions[[bbl_index]]$matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest_wider(col = propositions, names_sep = "_") |>
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_name = propositions_name,
         price = propositions_returnWin)

#==============================================================================
# Head to head
#==============================================================================

# Filter to head to head markets
head_to_head <-
  all_tab_markets |>
  filter(market_name == "Head To Head") |> 
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |> 
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades"))

# Write to csv
write_csv(head_to_head, "Data/scraped_odds/tab_h2h.csv")

#==============================================================================
# Player Runs Over / Under
#==============================================================================

# Filter to player runs over / under markets
player_runs_over_under <-
  all_tab_markets |>
  filter(market_name == "Player Runs") |> 
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |> 
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades"))

# Get Overs
player_runs_overs <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") |>
  mutate(line = str_remove(line, " Runs")) |>
  mutate(line = as.numeric(line)) |> 
  mutate(player_name = case_when(player_name == "M Labschagne" ~ "M Labuschagne",
                                 player_name == "J F McGurk" ~ "J Fraser-McGurk",
                                 player_name == "T Rogers" ~ "T F Rogers",
                            .default = player_name)) |>
  left_join(player_teams_initials[,c("player_name", "player_name_full", "player_team")]) |> 
  select(-player_name) |> 
  rename(over_price = price, player_name = player_name_full) |> 
  separate(match, into = c("home", "away"), sep = " v ", remove = FALSE) |>
  mutate(opposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home))

# Get Unders
player_runs_unders <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " Under ") |>
  mutate(line = str_remove(line, " Runs")) |>
  mutate(line = as.numeric(line)) |> 
  mutate(player_name = case_when(player_name == "M Labschagne" ~ "M Labuschagne",
                                 player_name == "J F McGurk" ~ "J Fraser-McGurk",
                                 player_name == "T Rogers" ~ "T F Rogers",
                                 .default = player_name)) |>
  left_join(player_teams_initials[,c("player_name", "player_name_full", "player_team")]) |> 
  select(-player_name) |> 
  rename(under_price = price, player_name = player_name_full) |> 
  separate(match, into = c("home", "away"), sep = " v ", remove = FALSE) |>
  mutate(opposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home))
# Combine
player_runs_over_under <-
  player_runs_overs |>
  left_join(player_runs_unders) |>
  select(
    match,
    market = market_name,
    home_team = home,
    away_team = away,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price
  ) |> 
  mutate(agency = "TAB")
  
#==============================================================================
# Player Runs Alternate Lines
#==============================================================================

# Filter to player runs alt line markets
player_runs_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "^To Score")) |>
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |>
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(
    player_name = case_when(
      player_name == "Tom Rogers" ~ "Tom F Rogers",
      player_name == "Jake Fraser McGurk" ~ "Jake Fraser-McGurk",
      .default = player_name
    )
  ) |>
  left_join(player_teams[,c("player_name", "player_team")]) |> 
  rename(over_price = price) |>
  separate(
    match,
    into = c("home", "away"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(opposition_team = case_when(player_team == home ~ away,
                                       player_team == away ~ home)) |>
  transmute(
    match,
    market = "Player Runs",
    home_team = home,
    away_team = away,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price
  )

# Combine all player runs and write out-----------------------------------------
player_runs <-
  player_runs_over_under |>
  bind_rows(player_runs_alt) |>
  mutate(agency = "TAB") |> 
  arrange(match, player_name, line)

player_runs |>
  write_csv("Data/scraped_odds/tab_player_runs.csv")

#==============================================================================
# Player Wickets Alternate Lines
#==============================================================================

# Filter to player wickets alt line markets
player_wickets_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "^To Take")) |>
  mutate(market_name = str_replace(market_name, " A ", " 1+ ")) |> 
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |>
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(
    player_name = case_when(
      player_name == "Matthew Kuhnemann" ~ "Matt Kuhnemann",
      .default = player_name
    )
  ) |>
  left_join(player_teams[,c("player_name", "player_team")]) |> 
  rename(over_price = price) |>
  separate(
    match,
    into = c("home", "away"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(opposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home)) |>
  transmute(
    match,
    market = "Player Wickets",
    home_team = home,
    away_team = away,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    agency = "TAB"
  )

# Combine all player wickets and write out-----------------------------------------
player_wickets_alt |> 
  write_csv("Data/scraped_odds/tab_player_wickets.csv")

#==============================================================================
# Player Boundaries Alternate Lines
#==============================================================================

# Filter to player boundaries alt line markets
player_boundaries_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "To Hit")) |>
  filter(str_detect(market_name, "To Hit a Four and a Six", negate = TRUE)) |>
  mutate(market_name = str_replace(market_name, " A ", " 1+ ")) |> 
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |>
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(
    player_name = case_when(
      player_name == "Tom Rogers" ~ "Tom F Rogers",
      player_name == "Jake Fraser McGurk" ~ "Jake Fraser-McGurk",
      .default = player_name
    )
  ) |>
  left_join(player_teams[,c("player_name", "player_team")]) |> 
  rename(over_price = price) |>
  separate(
    match,
    into = c("home", "away"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(opposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home)) |>
  mutate(market_name = if_else(str_detect(market_name, "Four"), "Number of 4s", "Number of 6s")) |>
  transmute(
    match,
    market = market_name,
    home_team = home,
    away_team = away,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    agency = "TAB"
  )

# Combine all player boundaries and write out-----------------------------------------
player_boundaries_alt |> 
  write_csv("Data/scraped_odds/tab_player_boundaries.csv")
