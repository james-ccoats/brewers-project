# Finding contract value for X
library(tidyverse)

set.seed(101)
n_sim <- 1000000

seasons <- 2026:2033
war_proj <- c(3.2, 3.5, 3.7, 3.4, 3.2, 3.0, 2.8, 2.6)

# Year-to-Year salaries
yy_base <- c(0.80, 0.85, 0.90, NA, NA, NA)
salary_if_injured_prev <- c(NA, NA, NA, 2.80, 5.25, 9.10)
salary_if_healthy_prev <- c(NA, NA, NA, 4.80, 9.00, 15.60)

# Guaranteed contract (first 6 years)
guaranteed <- c(2, 2.5, 4.5, 7.5, 10.5, 14.5)

# Parameters
p_injury <- 0.2
war_reduction <- 1.5
dollars_per_war <- 8

# year to year sim
sim_yty <- function(){
  total_cost <- 0
  total_war <- 0
  injured_prev <- FALSE
  
  for (i in 1:6){
    injured <- runif(1) < p_injury
    
    if (i <= 3){
      total_cost <- total_cost + yy_base[i]
    } else {
      total_cost <- total_cost + ifelse(injured_prev, salary_if_injured_prev[i], salary_if_healthy_prev[i])
    }
    
    state_war <- war_proj[i]
    if (injured_prev) state_war <- state_war - war_reduction
    total_war <- total_war + ifelse(injured, 0, state_war)
    
    injured_prev <- injured
  }
  return(list(cost = total_cost, war = total_war))
}

#sim guaranteed contract
sim_guarantee <- function(X){
  total_cost <- sum(guaranteed[1:6]) + 2 * X
  total_war <- 0
  injured_prev <- FALSE
  
  for (i in 1:8){
    injured <- runif(1) < p_injury
    state_war <- war_proj[i]
    if (injured_prev) state_war <- state_war - war_reduction
    total_war <- total_war + ifelse(injured, 0, state_war)
    injured_prev <- injured
  }
  
  return(list(cost = total_cost, war = total_war))
}

#simulation
ytoy_sims <- replicate(n_sim, sim_yty(), simplify = FALSE)
ytoy_costs <- sapply(ytoy_sims, function(x) x$cost)
ytoy_war <- sapply(ytoy_sims, function(x) x$war)
ytoy_net <- ytoy_war * dollars_per_war - ytoy_costs
ytoy_mean_net <- mean(ytoy_net)

#solve for X
solve_X <- function(){
  objective <- function(X){
    guaranteed_sims <- replicate(n_sim, sim_guarantee(X), simplify = FALSE)
    guaranteed_costs <- sapply(guaranteed_sims, function(x) x$cost)
    guaranteed_war <- sapply(guaranteed_sims, function(x) x$war)
    guaranteed_net <- guaranteed_war * dollars_per_war - guaranteed_costs
    
    (mean(guaranteed_net) - ytoy_mean_net)^2    
  } 
  optimize(objective, interval = c(0, 30))$minimum
}
breakeven <- solve_X()
print(breakeven)

