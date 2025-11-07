# Finding contract value for X
library(tidyverse)
library(ggplot2)

set.seed(101)
#cut down on n_sim to reduce run time. 10,000 yields similar to 1,000,000 with signifcantly less run time.
n_sim <- 10000
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
value_per_war <- 8

# year to year sim
sim_yty <- function(){
  total_cost <- 0
  total_war <- 0
  injured_prev <- FALSE
  active <- TRUE
  years_played <- 0
  net_values <- c()  
  
  for (i in 1:6){
    if (!active){
      # No contract offered, all remaining years are zeros
      next
    }
    
    injured <- runif(1) < p_injury
    
    # Determine cost
    if (i <= 3){
      cost_i <- yy_base[i]
    } else {
      cost_i <- ifelse(injured_prev, salary_if_injured_prev[i], salary_if_healthy_prev[i])
    }
    
    # Determine WAR
    state_war <- war_proj[i]
    if (injured_prev) state_war <- state_war - war_reduction
    war_i <- ifelse(injured, 0, state_war)
    
    # Update totals
    total_cost <- total_cost + cost_i
    total_war <- total_war + war_i
    years_played <- years_played + 1
    
    # Compute yearly net value
    net_value_year <- (war_i * value_per_war) - cost_i
    net_values <- c(net_values, net_value_year)
    
    # Check rolling average of the last 2 years
    recent_values <- tail(net_values, 2)
    if (mean(recent_values) < 0){
      active <- FALSE
    }
    
    injured_prev <- injured
  }
  
  return(list(cost = total_cost, war = total_war, years_played = years_played))
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
ytoy_net <- ytoy_war * value_per_war - ytoy_costs
ytoy_mean_net <- mean(ytoy_net)

#solve for X
solve_X <- function(){
  objective <- function(X){
    guaranteed_sims <- replicate(n_sim, sim_guarantee(X), simplify = FALSE)
    guaranteed_costs <- sapply(guaranteed_sims, function(x) x$cost)
    guaranteed_war <- sapply(guaranteed_sims, function(x) x$war)
    guaranteed_net <- guaranteed_war * value_per_war - guaranteed_costs
    
    (mean(guaranteed_net) - ytoy_mean_net)^2    
  } 
  optimize(objective, interval = c(0, 30))$minimum
}
breakeven <- solve_X()
print(breakeven)

#calculated guaranteed values outside of function just to have for plotting
guaranteed_sims <- replicate(n_sim, sim_guarantee(breakeven), simplify = FALSE)
guaranteed_costs <- sapply(guaranteed_sims, function(x) x$cost)
guaranteed_war <- sapply(guaranteed_sims, function(x) x$war)
guaranteed_net <- guaranteed_war * value_per_war - guaranteed_costs

print(mean(guaranteed_net))
print(ytoy_mean_net)

plot_df <- data.frame(ytoy_net_value, guaranteed_net)

plot_df_long <- plot_df %>%
  pivot_longer(cols = everything(), names_to = "contract", values_to = "net_value")

#density plot
ggplot(plot_df_long, aes(x = net_value, fill = contract)) +
  geom_vline(aes(xintercept = mean(ytoy_net_value)), color = "blue", linetype = "dotted", linewidth = 1) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Net Values",
    x = "Net Value ($ millions)",
    y = "Density"
  ) +
  theme_minimal()

#create breakeven curve
breakeven_curve <- function(){
  X_values <- seq(0, 30, by = 0.5)
  
  results <- sapply(X_values, function(X){
    guaranteed_sims <- replicate(n_sim, sim_guarantee(X), simplify = FALSE)
    guaranteed_war <- sapply(guaranteed_sims, function(x) x$war)
    guaranteed_costs <- sapply(guaranteed_sims, function(x) x$cost)
    mean(guaranteed_war * value_per_war - guaranteed_costs)
  })
  
  data.frame(X = X_values, mean_net = results)
}

curve_df <- breakeven_curve()

#plot breakeven curve
ggplot(curve_df, aes(x = X, y = mean_net)) +
  geom_line(size = 0.8, color = "steelblue") +
  geom_hline(yintercept = ytoy_mean_net, linetype = "dashed", color = "red") +
  geom_vline(xintercept = breakeven, linetype = "dotted", color = "darkgreen") +
  annotate("text", x = breakeven + 1, y = ytoy_mean_net + 5,
           label = paste0("Breakeven X = ", round(breakeven, 2), "M"),
           color = "darkgreen", fontface = "bold", hjust = 0) +
  labs(
    title = "Breakeven Contract Value (X)",
    x = "Additional Year Salary (X, in $ millions)",
    y = "Expected Net Value ($ millions)"
  ) +
  theme_minimal(base_size = 14)

