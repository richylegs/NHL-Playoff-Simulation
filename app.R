##### Libraries #####
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

##### Game Simulation Function #####
sim_game <- function(exp_goals_t1, exp_goals_t2){
  # Simulate goals for team 1 and team 2 using Poisson distribution
  goals_t1 <- rpois(1, lambda = exp_goals_t1)
  goals_t2 <- rpois(1, lambda = exp_goals_t2)
  
  # Compare goals and determine the winner
  if (goals_t1 > goals_t2) {
    return("team1")
  } else if (goals_t2 > goals_t1) {
    return("team2")
  } else {
    # If tied, pick a winner randomly with weighted probability
    prob_team1_wins <- exp_goals_t1 / (exp_goals_t1 + exp_goals_t2)
    return(ifelse(runif(1) < prob_team1_wins, "team1", "team2"))
  }
}

##### Series Simulation Function #####
sim_series <- function(team1, team2){
  # Convert abbreviation
  team1 <- toupper(team1)
  team2 <- toupper(team2)
  
  # Pull GF/G Team 1
  gf_t1 <- df %>%
    filter(Abbrev == team1) %>%
    pull(`GF/G`)
  
  # Pull GF/G Team 2
  gf_t2 <- df %>%
    filter(Abbrev == team2) %>%
    pull(`GF/G`)
  
  # Pull GA/G Team 1
  ga_t1 <- df %>%
    filter(Abbrev == team1) %>%
    pull(`GA/G`)
  
  # Pull GA/G Team 2
  ga_t2 <- df %>%
    filter(Abbrev == team2) %>%
    pull(`GA/G`)
  
  # Expected Goals Team 1
  exp_goals_t1 <- (gf_t1 + ga_t2)/2
  
  # Expected Goals Team 2
  exp_goals_t2 <- (gf_t2 + ga_t1)/2
  
  # Initialize win counts
  team1_wins <- 0
  team2_wins <- 0
  games_played <- 0
  
  # Simulate games until one team wins 4 times
  while (team1_wins < 4 && team2_wins < 4) {
    # Simulate a single game
    result <- sim_game(exp_goals_t1, exp_goals_t2)
    games_played <- games_played + 1
    
    # Update win count based on result
    if (result == "team1") {
      team1_wins <- team1_wins + 1
    } else {
      team2_wins <- team2_wins + 1
    }
  }
  
  # Return winner and number of games played
  if (team1_wins == 4) {
    return(list(winner = team1, games = games_played))
  } else {
    return(list(winner = team2, games = games_played))
  }
}

##### Simulate 1000 Times Function #####
sim_1000 <- function(team1, team2){
  # Initialize result table for tracking wins
  result_table <- matrix(0, nrow = 2, ncol = 4, dimnames = list(c(team1, team2), c(4, 5, 6, 7)))
  
  # Track total wins for each team
  total_wins <- c(team1 = 0, team2 = 0)
  names(total_wins) <- c(team1, team2)
  
  # Simulate 1000 times
  for (i in 1:1000) {
    result <- sim_series(team1, team2)
    winner <- result$winner
    games <- result$games
    
    # Increment the appropriate cell in the table
    result_table[winner, as.character(games)] <- result_table[winner, as.character(games)] + 1
    
    # Track the total wins
    total_wins[winner] <- total_wins[winner] + 1
  }
  
  # Calculate win probabilities in percentage
  win_probabilities <- total_wins / 1000 * 100
  
  # Return both the result table and win probabilities
  return(list(
    result_table = result_table,
    win_probabilities = win_probabilities
  ))
}

##### Graphic Function ######
plot_simulation_results <- function(simulation_results) {
  # Convert result_table to a data frame
  result_df <- as.data.frame(simulation_results$result_table)
  result_df$Team <- rownames(result_df) 
  
  # Reshape data for ggplot2
  result_df_long <- melt(result_df, id.vars = "Team", variable.name = "Games", value.name = "Wins")
  
  # Plot bar chart
  ggplot(result_df_long, aes(x = Games, y = Wins, fill = Team)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Wins), position = position_dodge(width = 0.9), vjust = -0.5) +
    scale_fill_manual(values = team_color) +
    labs(title = "Distribution of Wins by Number of Games", 
         x = "Number of Games", 
         y = "Number of Wins",
         fill = "Team") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
    )
}

##### Import & Clean Data #####
df <- read_csv("NHL_2023-24_Reg.csv", show_col_types = FALSE)

# Set Abbreviations
team_abbreviations <- c(
  "New York Rangers" = "NYR",
  "Dallas Stars" = "DAL",
  "Carolina Hurricanes" = "CAR",
  "Winnipeg Jets" = "WPG",
  "Florida Panthers" = "FLA",
  "Boston Bruins" = "BOS",
  "Vancouver Canucks" = "VAN",
  "Colorado Avalanche" = "COL",
  "Edmonton Oilers" = "EDM",
  "Toronto Maple Leafs" = "TOR",
  "Nashville Predators" = "NSH",
  "Los Angeles Kings" = "LAK",
  "Tampa Bay Lightning" = "TBL",
  "Vegas Golden Knights" = "VGK",
  "New York Islanders" = "NYI",
  "St. Louis Blues" = "STL",
  "Washington Capitals" = "WSH",
  "Detroit Red Wings" = "DET",
  "Pittsburgh Penguins" = "PIT",
  "Minnesota Wild" = "MIN",
  "Philadelphia Flyers" = "PHI",
  "Buffalo Sabres" = "BUF",
  "New Jersey Devils" = "NJD",
  "Seattle Kraken" = "SEA",
  "Calgary Flames" = "CAL",
  "Ottawa Senators" = "OTT",
  "Arizona Coyotes" = "ARZ",
  "Montreal Canadiens" = "MTL",
  "Columbus Blue Jackets" = "CBJ",
  "Anaheim Ducks" = "ANA",
  "Chicago Blackhawks" = "CHI",
  "San Jose Sharks" = "SJS"
)

# Keep necessary columns
df <- df %>%
  select(c("Team", "GF/G", "GA/G")) %>%
  mutate(Abbrev = team_abbreviations[Team])

# Set Colors
team_color <- c(
  "NYR" = "#0038A8",
  "DAL" = "#006847",
  "CAR" = "#CE1126",
  "WPG" = "#041E42",
  "FLA" = "#041E42",
  "BOS" = "#FFB81C",
  "VAN" = "#00843d",
  "COL" = "#6F263D",
  "EDM" = "#FF4C00",
  "TOR" = "#00205b",
  "NSH" = "#FFB81C",
  "LAK" = "#111111",
  "TBL" = "#002868",
  "VGK" = "#B4975A",
  "NYI" = "#00539b",
  "STL" = "#002F87",
  "WSH" = "#C8102E",
  "DET" = "#ce1126",
  "PIT" = "#FCB514",
  "MIN" = "#154734",
  "PHI" = "#F74902",
  "BUF" = "#003087",
  "NJD" = "#CE1126",
  "SEA" = "#99d9d9",
  "CAL" = "#D2001C",
  "OTT" = "#DA1A32",
  "ARZ" = "#8C2633",
  "MTL" = "#AF1E2D",
  "CBJ" = "#002654",
  "ANA" = "#F47A38",
  "CHI" = "#CF0A2C",
  "SJS" = "#006D75"
)

##### UI #####
ui <- fluidPage(
  # Application title
  titlePanel("2024 NHL Playoff Series Simulation"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting Team 1
      selectInput("team1", "Select Team 1:", 
                  choices = sort(unique(df$Team)),
                  selected = "Florida Panthers"),
      
      # Dropdown for selecting Team 2
      selectInput("team2", "Select Team 2:", 
                  choices = sort(unique(df$Team)),
                  selected = "Edmonton Oilers"),
      
      # Button to run the simulation
      actionButton("run_sim", "Run 1000 Simulations"),
      
      # Add line break
      br(), br(),
      
      # Data Source
      p("Goals For and Against per Game Data Provided by Hockey Reference: ", 
        a("https://www.hockey-reference.com/leagues/NHL_2024.html#stats", 
          href = "https://www.hockey-reference.com/leagues/NHL_2024.html#stats",
          target = "_blank"))
    ),
    
    # Main panel to display the output
    mainPanel(
      # Plot output
      plotOutput("simulation_plot", height = "700px"),
      
      # Title for the win probabilities table
      tags$h3(style = "font-weight: bold;", "Probability of Winning the Series"),
      
      # Table output for win probabilities
      tableOutput("win_prob_table")
    )
  )
)

##### Server #####
server <- function(input, output) {
  # Reactive variable to store simulation results
  simulation_results <- reactiveVal(NULL)
  
  # Reactive variable to store selected team names
  selected_teams <- reactiveVal(c(NULL, NULL))
  
  # Reactive expression to run simulation when button is clicked
  observeEvent(input$run_sim, {
    # Get the selected teams
    selected_team1 <- input$team1
    selected_team2 <- input$team2
    
    # Save the selected teams in the reactive variable
    selected_teams(c(selected_team1, selected_team2))
    
    # Convert team names to abbreviations
    abbrev_team1 <- df %>%
      filter(Team == selected_team1) %>%
      pull(Abbrev)
    
    abbrev_team2 <- df %>%
      filter(Team == selected_team2) %>%
      pull(Abbrev)
    
    # Run simulation and store results in the reactive variable
    results <- sim_1000(abbrev_team1, abbrev_team2)
    simulation_results(results)
    
    # Generate the plot using the plot_simulation_results function
    output$simulation_plot <- renderPlot({
      plot_simulation_results(simulation_results())
    })
    
    # Render the win probabilities table
    output$win_prob_table <- renderTable({
      req(simulation_results())
      
      # Create a data frame with formatted win probabilities
      win_prob_df <- data.frame(
        Team = selected_teams()[1:2],
        Probability = sprintf("%.1f%%", simulation_results()$win_probabilities)
      )
      
      # Return the data frame
      return(win_prob_df)
    })
    
    # Set the title for the table
    output$win_prob_table_title <- renderText({
      "Probability of Winning the Series"
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
