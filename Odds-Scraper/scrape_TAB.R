# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Get player names--------------------------------------------------------------
bbl_player_names_23 <-
  read_rds("Data/player_names_teams_bbl.rds") |> 
  filter(season == "2023/24") |> 
  select(-season)

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

# Map functions to data
all_tab_markets <-
  map(tab_response$competitions[[1]]$matches, get_match_info) |> bind_rows()

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
  filter(str_detect(prop_name, "over")) |>
  separate(prop_name, into = c("player", "line"), sep = " over ") |>
  mutate(line = str_remove(line, " runs")) |>
  mutate(line = as.numeric(line)) |> 
  mutate(player = case_when(player == "G Harris" ~ "GM Harris",
                            player == "C Athapthu" ~ "AC Jayangani",
                            player == "K Mack" ~ "KM Mack",
                            player == "S Devine" ~ "SFM Devine",
                            player == "B Mooney" ~ "BL Mooney",
                            player == "H Mathews" ~ "HK Matthews",
                            player == "T Beaumont" ~ "TT Beaumont",
                            player == "M Bouchier" ~ "ME Bouchier",
                            player == "S Dunkley" ~ "SIR Dunkley",
                            .default = player)) |>
  left_join(bbl_player_names_23) |> 
  rename(over_price = price, player_team = team) |> 
  separate(match, into = c("home", "away"), sep = " v ", remove = FALSE) |>
  mutate(oppposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home))

# Get Unders
player_runs_unders <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "under")) |>
  separate(prop_name, into = c("player", "line"), sep = " under ") |>
  mutate(line = str_remove(line, " runs")) |>
  mutate(line = as.numeric(line)) |> 
  mutate(player = case_when(player == "G Harris" ~ "GM Harris",
                            player == "C Athapthu" ~ "AC Jayangani",
                            player == "K Mack" ~ "KM Mack",
                            player == "S Devine" ~ "SFM Devine",
                            player == "B Mooney" ~ "BL Mooney",
                            player == "H Mathews" ~ "HK Matthews",
                            player == "T Beaumont" ~ "TT Beaumont",
                            player == "M Bouchier" ~ "ME Bouchier",
                            player == "S Dunkley" ~ "SIR Dunkley",
                            .default = player)) |>
  left_join(bbl_player_names_23) |> 
  rename(under_price = price, player_team = team) |> 
  separate(match, into = c("home", "away"), sep = " v ", remove = FALSE) |>
  mutate(oppposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home))
# Combine
player_runs_over_under <-
  player_runs_overs |>
  left_join(player_runs_unders) |>
  select(
    match,
    market_name,
    home_team = home,
    away_team = away,
    player,
    player_team,
    oppposition_team,
    line,
    over_price,
    under_price
  )
  
#==============================================================================
# Player Runs Alternate Lines
#==============================================================================

# Filter to player runs alt line markets
player_runs_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "To Score")) |>
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |>
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player = str_remove(prop_name, " \\(.*\\)")) |>
  separate(player,
           into = c("first_name", "last_name"),
           sep = " ") |>
  mutate(first_initial = str_sub(first_name, 1, 1)) |>
  mutate(player = paste(first_initial, last_name, sep = " ")) |>
  mutate(
    player = case_when(
      player == "G Harris" ~ "GM Harris",
      player == "C Athapthu" ~ "AC Jayangani",
      player == "K Mack" ~ "KM Mack",
      player == "S Devine" ~ "SFM Devine",
      player == "B Mooney" ~ "BL Mooney",
      player == "H Mathews" ~ "HK Matthews",
      player == "T Beaumont" ~ "TT Beaumont",
      player == "M Bouchier" ~ "ME Bouchier",
      player == "S Dunkley" ~ "SIR Dunkley",
      .default = player
    )
  ) |>
  left_join(bbl_player_names_23) |>
  rename(over_price = price, player_team = team) |>
  separate(
    match,
    into = c("home", "away"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(oppposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home)) |>
  select(
    match,
    market_name,
    home_team = home,
    away_team = away,
    player,
    player_team,
    oppposition_team,
    line,
    over_price
  )


#==============================================================================
# Player Wickets Alternate Lines
#==============================================================================

# Filter to player wickets alt line markets
player_wickets_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "To Take")) |>
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |>
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades")) |>
  mutate(market_name = str_replace(market_name, "To Take A Wicket", "To Take 1+ Wickets")) |> 
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player = str_remove(prop_name, " \\(.*\\)")) |>
  separate(player,
           into = c("first_name", "last_name"),
           sep = " ") |>
  mutate(first_initial = str_sub(first_name, 1, 1)) |>
  mutate(player = paste(first_initial, last_name, sep = " ")) |>
  mutate(
    player = case_when(
      player == "A Edgar" ~ "AL Edgar",
      player == "A Kerr" ~ "AC Kerr",
      player == "H Darlington" ~ "HJ Darlington",
      player == "J Jonassen" ~ "JL Jonassen",
      player == "M Schutt" ~ "ML Schutt",
      player == "S Coyte" ~ "SJ Coyte",
      player == "H Mathews" ~ "HK Matthews",
      .default = player
    )
  ) |>
  left_join(bbl_player_names_23) |>
  rename(over_price = price, player_team = team) |>
  separate(
    match,
    into = c("home", "away"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(oppposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home)) |>
  select(
    match,
    market_name,
    home_team = home,
    away_team = away,
    player,
    player_team,
    oppposition_team,
    line,
    over_price
  )

#==============================================================================
# Player Boundaries Alternate Lines
#==============================================================================

# Filter to player boundaries alt line markets
player_boundaries_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "To Hit")) |>
  mutate(match = str_replace_all(match, "AdelaideStrikers", "Adelaide Strikers")) |>
  mutate(match = str_replace_all(match, "Melb Renegades", "Melbourne Renegades")) |>
  mutate(market_name = str_replace(market_name, "To Hit A Four", "To Hit 1+ Fours")) |> 
  mutate(market_name = str_replace(market_name, "To Hit A Six", "To Hit 1+ Sixes")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player = str_remove(prop_name, " \\(.*\\)")) |>
  separate(player,
           into = c("first_name", "last_name"),
           sep = " ") |>
  mutate(first_initial = str_sub(first_name, 1, 1)) |>
  mutate(player = paste(first_initial, last_name, sep = " ")) |>
  mutate(
    player = case_when(
      player == "G Harris" ~ "GM Harris",
      player == "C Athapthu" ~ "AC Jayangani",
      player == "K Mack" ~ "KM Mack",
      player == "S Devine" ~ "SFM Devine",
      player == "B Mooney" ~ "BL Mooney",
      player == "H Mathews" ~ "HK Matthews",
      player == "T Beaumont" ~ "TT Beaumont",
      player == "M Bouchier" ~ "ME Bouchier",
      player == "S Dunkley" ~ "SIR Dunkley",
      .default = player
    )
  ) |>
  left_join(bbl_player_names_23) |>
  rename(over_price = price, player_team = team) |>
  separate(
    match,
    into = c("home", "away"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(oppposition_team = case_when(player_team == home ~ away,
                                      player_team == away ~ home)) |>
  select(
    match,
    market_name,
    home_team = home,
    away_team = away,
    player,
    player_team,
    oppposition_team,
    line,
    over_price
  )

