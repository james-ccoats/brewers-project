library(dplyr)
library(ggplot2)

set.seed(123)

teams <- data.frame(
  team = c("Brewers","Phillies","Dodgers","Cubs","Padres","Mets","Reds","Diamondbacks"),
  wins = c(95,92,88,88,86,80,80,79),
  losses = c(62,64,68,68,71,76,76,77)
) %>%
  mutate(games_played = wins + losses,
         games_left = 162 - games_played)


potential_opponents <- setdiff(teams$team, "Brewers") # all teams except Brewers
prob_vs_brewers <- setNames(rep(0, length(potential_opponents)), potential_opponents)


simulate_season_brewers <- function(){
  
  w <- teams$wins
  
  # Key matchups
  dodgers_vs_dbacks <- rbinom(1,3,0.5)
  w[teams$team=="Dodgers"] <- w[teams$team=="Dodgers"] + dodgers_vs_dbacks
  w[teams$team=="Diamondbacks"] <- w[teams$team=="Diamondbacks"] + (3 - dodgers_vs_dbacks)
  
  cubs_vs_mets <- rbinom(1,3,0.5)
  w[teams$team=="Cubs"] <- w[teams$team=="Cubs"] + cubs_vs_mets
  w[teams$team=="Mets"] <- w[teams$team=="Mets"] + (3 - cubs_vs_mets)
  
  brewers_vs_padres <- rbinom(1,2,0.5)
  w[teams$team=="Brewers"] <- w[teams$team=="Brewers"] + (2 - brewers_vs_padres)
  w[teams$team=="Padres"] <- w[teams$team=="Padres"] + brewers_vs_padres
  
  # Remaining games
  w[teams$team=="Dodgers"] <- w[teams$team=="Dodgers"] + rbinom(1,3,0.5)
  w[teams$team=="Cubs"] <- w[teams$team=="Cubs"] + rbinom(1,3,0.5)
  w[teams$team=="Padres"] <- w[teams$team=="Padres"] + rbinom(1,3,0.5)
  w[teams$team=="Brewers"] <- w[teams$team=="Brewers"] + rbinom(1,3,0.5)
  w[teams$team=="Mets"] <- w[teams$team=="Mets"] + rbinom(1,3,0.5)
  w[teams$team=="Phillies"] <- w[teams$team=="Phillies"] + rbinom(1,6,0.5)
  w[teams$team=="Reds"] <- w[teams$team=="Reds"] + rbinom(1,6,0.5)
  w[teams$team=="Diamondbacks"] <- w[teams$team=="Diamondbacks"] + rbinom(1,3,0.5)
  
  # Determine standings
  standings <- data.frame(team=teams$team, wins=w, order=1:8)
  standings <- standings %>% arrange(desc(wins), order)
  
  # Division winners
  seeds <- standings$team[1:3]
  
  # Wild Card seeds
  wc4 <- standings$team[4]
  wc5 <- standings$team[5]
  
  # Brewers seed
  brewers_seed <- which(standings$team=="Brewers")
  
  # Identify potential DS opponent
  for(team in potential_opponents){
    if(team %in% c(wc4,wc5)){
      if(wc4 == team){ p_series <- 0.55^2 + 2*0.55^2*0.45 }  # higher seed wins WC
      else { p_series <- 0.45^2 + 2*0.45^2*0.55 }             # lower seed
    } else {
      p_series <- 0
    }
    
    # Check if Brewers play this team in DS
    if(brewers_seed == 1 && team %in% c(wc4,wc5)) {
      prob_vs_brewers[team] <<- prob_vs_brewers[team] + p_series
    } else if(brewers_seed == 2) {
      if(team == standings$team[3] || team == standings$team[6]){
        if(standings$team[3] == team){
          p_series_36 <- 0.55^2 + 2*0.55^2*0.45
        } else {
          p_series_36 <- 0.45^2 + 2*0.45^2*0.55
        }
        prob_vs_brewers[team] <<- prob_vs_brewers[team] + p_series_36
      }
    }
  }
}

n_sim <- 1e5
prob_vs_brewers <- setNames(rep(0, length(potential_opponents)), potential_opponents)

for(i in 1:n_sim){
  simulate_season_brewers()
}

# Convert counts to probabilities
prob_vs_brewers <- prob_vs_brewers / n_sim

# Bar graph
prob_df <- data.frame(
  team = names(prob_vs_brewers),
  probability = as.numeric(prob_vs_brewers)
) %>% arrange(desc(probability))

ggplot(prob_df, aes(x = reorder(team, probability), y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Brewers Opponent Probability in DS",
    x = "Team",
    y = "Probability"
  ) +
  theme_minimal(base_size = 14)

print(prob_vs_brewers)
