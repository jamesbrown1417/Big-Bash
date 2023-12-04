# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/twenty20-big-bash"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
  
  # Get data from main market page
  matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Function to get odds
  get_odds <- function(match) {
    odds <-
      match |>
      html_nodes(".priceTextSize_frw9zm9") |>
      html_text() |>
      as.numeric()
    
    # Home team
    home_win <- odds[2]
    away_win <- odds[1]
    
    # Output
    tibble(home_win, away_win)
  }
  
  # Function to get start time
  get_start_time <- function(match) {
    start_time <-
      match |>
      html_nodes(".oneLine_f15ay66x") |>
      html_text()
    
    # Output
    tibble(start_time)
  }
  
  # Map functions to each match and combine together
  all_main_market_data <-
    bind_cols(
      map(matches, get_team_names) |> bind_rows(),
      map(matches, get_odds) |> bind_rows(),
      map(matches, get_start_time) |> bind_rows()
    )
  
  #===============================================================================
  # Head to Head markets---------------------------------------------------------#
  #===============================================================================
  
  sportsbet_h2h <-
    all_main_market_data |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           start_time,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet") |>
    mutate(start_time = str_extract(start_time, "\\,.*")) |> 
    mutate(start_time = str_remove(start_time, "\\, ")) |>
    mutate(start_time = str_remove(start_time, " \\d{2}\\:\\d{2}")) |> 
    mutate(start_time = dmy(paste(start_time, "2023"))) |> 
    mutate(start_time = if_else(month(start_time) < 9, start_time + years(1), start_time))
  
  # Write to csv
  write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")
  
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[2]
    away_team <- team_names[1]
    
    # Output
    tibble(home_team, away_team)
  }
  
  
  # Get match links
  match_links <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".linkMultiMarket_fcmecz0") |> 
    html_attr("href")
  
  # Get match IDs from links
  match_ids <-
    match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()
  
  # Get data from main market page
  matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Get team names that correspond to each match link
  team_names <-
    map_dfr(matches, get_team_names) |> 
    bind_cols("match_id" = match_ids)
  
  # Get all links
  top_markets_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/304/Markets")
  run_scorer_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/306/Markets")
  top_wicket_takers_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/307/Markets")
  first_innings_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/325/Markets")
  
  # Function to read a url and get the player props-------------------------------
  
  read_prop_url <- function(url) {
    
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |> 
      resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
      for (selection in market$selections) {
        
        # Append to vectors
        prop_market_name = c(prop_market_name, market$name)
        selection_name_prop = c(selection_name_prop, selection$name)
        prop_market_selection = c(prop_market_selection, selection$resultType)
        prop_market_price = c(prop_market_price, selection$price$winPrice)
        player_id = c(player_id, selection$externalId)
        market_id = c(market_id, market$externalId)
        if (is.null(selection$unformattedHandicap)) {
          selection$unformattedHandicap = NA
          handicap = c(handicap, selection$unformattedHandicap)
        } else {
          selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
          handicap = c(handicap, selection$unformattedHandicap)
        }
      }
    }
    
    # Output
    tibble(prop_market_name,
           selection_name_prop,
           prop_market_selection,
           prop_market_price,
           player_id,
           market_id,
           handicap,
           url)
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)
  
  #===============================================================================
  # Top Run Scorers
  #===============================================================================
  
  # Map function to player points urls
  top_run_scorers_data <-
    map(top_markets_links, safe_read_prop_url)
  
  # Get just result part from output
  top_run_scorers_data <-
    top_run_scorers_data |>
    map("result") |>
    map_df(bind_rows) |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    left_join(team_names, by = "match_id") |> 
    mutate(match = paste(home_team, "v", away_team))
  
  # Get home team top runscorer
  home_top_runscorer <- 
    top_run_scorers_data |>
    filter(str_detect(prop_market_name, "Top .* Runscorer")) |> 
    mutate(player_team = str_remove(prop_market_name, "Top ")) |>
    mutate(player_team = str_remove(player_team, " Runscorer")) |> 
    filter(player_team == home_team) |>
    transmute(match,
              market = "Top Team Runscorer",
              home_team,
              away_team,
              player_name = selection_name_prop,
              player_team,
              opposition_team = away_team,
              price = prop_market_price)
    
  
  # Get away team top runscorer
  away_top_runscorer <- 
    top_run_scorers_data |>
    filter(str_detect(prop_market_name, "Top .* Runscorer")) |> 
    mutate(player_team = str_remove(prop_market_name, "Top ")) |>
    mutate(player_team = str_remove(player_team, " Runscorer")) |> 
    filter(player_team == away_team) |>
    transmute(match,
              market = "Top Team Runscorer",
              home_team,
              away_team,
              player_name = selection_name_prop,
              player_team,
              opposition_team = home_team,
              price = prop_market_price)
  
  # Combine
  top_team_runscorer <-
    bind_rows(home_top_runscorer, away_top_runscorer)
  
  # Player of the match
  player_of_the_match <- 
    top_run_scorers_data |>
    filter(str_detect(prop_market_name, "Player of the Match")) |> 
    transmute(match,
              market = prop_market_name,
              home_team,
              away_team,
              player_name = selection_name_prop,
              price = prop_market_price)
  
  #===============================================================================
  # Player Runs
  #===============================================================================
  
  # Map function to player points urls
  player_runs_data <-
    map(run_scorer_links, safe_read_prop_url)
  
  # Get just result part from output
  player_runs_data <-
    player_runs_data |>
    map("result") |>
    map_df(bind_rows) |> 
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |> 
    rename(match_id = url) |> 
    mutate(match_id = as.numeric(match_id)) |> 
    left_join(team_names, by = "match_id") |> 
    mutate(match = paste(home_team, "v", away_team))
  
  # Player runs over / under
  player_runs_overs <- 
    player_runs_data |>
    filter(str_detect(prop_market_name, "Total Runs")) |> 
    filter(str_detect(selection_name_prop, "Over")) |> 
    transmute(match,
              market = "Player Runs",
              home_team,
              away_team,
              player_name = str_remove(selection_name_prop, " - Over"),
              line = handicap,
              over_price = prop_market_price)
  
  #===============================================================================
  # Top Wicket Takers
  #===============================================================================
 
  # Map function to player points urls
  top_wicket_takers_data <-
    map(top_wicket_takers_links, safe_read_prop_url)
  
  # Get just result part from output
  top_wicket_takers_data <-
    top_wicket_takers_data |>
    map("result") |>
    map_df(bind_rows)
  
  #===============================================================================
  # First Innings
  #===============================================================================
  
  # Map function to player points urls
  first_innings_data <-
    map(first_innings_links, safe_read_prop_url)
  
  # Get just result part from output
  first_innings_data <-
    first_innings_data |>
    map("result") |>
    map_df(bind_rows)
}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
