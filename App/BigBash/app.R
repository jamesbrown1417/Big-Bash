library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

# Determine the operating system
os_type <- Sys.info()["sysname"]

#===============================================================================
# Load in data
#===============================================================================

batting_stats_bbl <- readRDS("../../Data/batting_stats_bbl.rds")
bowling_stats_bbl <- readRDS("../../Data/bowling_stats_bbl.rds")

past_two_seasons <-
  batting_stats_bbl |>
  filter(start_date >= "2020-06-10") |>
  group_by(match_id,
           # innings,
           # team,
           venue) |>
  summarise(
    runs = sum(runs_scored),
    balls = sum(balls_faced),
    fours = sum(fours),
    sixes = sum(sixes)
  )

past_two_seasons |>
  # filter(innings == 1) |> 
  group_by(venue) |>
  summarise(
    games_played = n(),
    avg_runs = mean(runs),
    avg_sixes = mean(sixes),
    avg_fours = mean(fours)
  ) |> 
  arrange(desc(avg_runs))

#===============================================================================
# Read in scraped odds
#===============================================================================

# Conditional logic for loading data based on OS
if (os_type == "Windows") {
  # Read RDS Data for Windows
  player_runs_data <- read_rds("../../Data/processed_odds/all_player_runs.rds")
  player_wickets_data <- read_rds("../../Data/processed_odds/all_player_wickets.rds")
  player_boundaries_data <- read_rds("../../Data/processed_odds/all_player_boundaries.rds")
} else {
  # Google Sheets Data for other OS
  ss_name <- gs4_find("Big Bash Data")
  player_runs_data <- read_sheet(ss = ss_name, sheet = "Player Runs")
  player_wickets_data <- read_sheet(ss = ss_name, sheet = "Player Wickets")
  player_boundaries_data <- read_sheet(ss = ss_name, sheet = "Player Boundaries")
}


#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
  title = "Big Bash",
  selected = "Odds Screen",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tags$head(
    tags$style(HTML("
      .tab-content, .tab-pane {
        height: 1250px;
        overflow-y: auto;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
    "))
  ),
  nav_panel(
    title = "Odds Screen",
    grid_container(
      layout = c("odds_screen odds_table"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(area = "odds_screen",
                card_header("Settings"),
                card_body(
                  # Add your specific input fields here
                  # Example: selectInput, textInput, numericInput, etc.
                  selectInput(
                    inputId = "agency_input",
                    label = "Select Agencies:",
                    choices = player_runs_data$agency |> unique(),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = player_runs_data$agency |> unique(),
                  ),
                  selectInput(
                    inputId = "market_input",
                    label = "Select Market:",
                    choices = c("Runs", "Wickets", "Boundaries"),
                    multiple = FALSE
                  ),
                  selectInput(
                    inputId = "match_input",
                    label = "Select Matches:",
                    choices = player_runs_data$match |> unique(),
                    multiple = TRUE,
                    selectize = FALSE,
                    selected = player_runs_data$match |> unique()
                  ),
                  textInput(
                    inputId = "player_name_input_b",
                    label = "Select Player:",
                    value = NA
                  ),
                  checkboxInput(
                    inputId = "only_unders",
                    label = "Only Show Markets With Unders",
                    value = FALSE
                  ),
                  checkboxInput(
                    inputId = "only_best",
                    label = "Only Show Best Market Odds - Overs",
                    value = FALSE
                  ),
                  checkboxInput(
                    inputId = "only_best_unders",
                    label = "Only Show Best Market Odds - Unders",
                    value = FALSE
                  )
                )),
      grid_card(area = "odds_table",
                card_body(
                  DTOutput(outputId = "scraped_odds_table", height = "1000px")
                ))
    )
  )
  # You can add more nav_panels here if needed for other sections of your new project
)

#===============================================================================
# Server
#===============================================================================

server <- function(input, output, session) {
  # Add your server-side code here
  # Reactive function to scrape odds
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------
    
    # Runs
    if (input$market_input == "Runs") {
      odds <-
        player_runs_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Boundaries
    if (input$market_input == "Boundaries") {
      odds <-
        player_boundaries_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match) 
    }
    
    # Wickets
    if (input$market_input == "Wickets") {
      odds <-
        player_wickets_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    if (input$only_best == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(over_price)) |>
        group_by(player_name, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    if (input$only_best_unders == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(under_price)) |>
        group_by(player_name, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    # Return odds
    return(odds)
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
              fillContainer = TRUE,
              filter = "top",
              options = list(
                pageLength = 17,
                autoWidth = FALSE,
                scrollX = TRUE, scrollY = TRUE,
                lengthMenu = c(5, 10, 15, 20, 25, 30)
              ))
  })
}

# Run the application
shinyApp(ui, server)
