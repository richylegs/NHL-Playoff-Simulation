https://richardlegler.shinyapps.io/Simulate_NHL_Series/

NHL_2023-24_Reg.csv data provided by Hockey Reference (https://www.hockey-reference.com/leagues/NHL_2024.html#stats) forthe 2023-24 regular season.

NHL Playoff Forecasting.Rmd contains base code of the R Shiny application. Functions to simulate each game of a playoff series using the goals for per game averaged with the opponents goals against per game as a poisson distribution. Matchups are simulated until one team wins four games with ties settled by a weighted probability of the distribution inputs for each team. Each matchup is then simulated 1000 times.

app.R is an R Shiny app to simulate any playoff matchup 1000 times.
