library(ggplot2)

set.seed(100)

advance_on_single <- function(bases){
  runs <- 0
  new_bases <- c(FALSE, FALSE, FALSE)
  
  #update runs and bases
  if (bases[3] == TRUE) runs <- runs + 1
  if (bases[2] == TRUE) runs <- runs + 1
  if (bases[1] == TRUE) new_bases[3] <- TRUE
  new_bases[1] <- TRUE
  
  return(list(new_bases = new_bases, runs = runs))
}

get_batter_probs <- function(batter, p) {
  switch(as.character(batter),
         "1"=c(SO=0.6, S=0.3, HR=0.1),
         "2"=c(SO=0.6, S=0.3, HR=0.1),
         "3"=c(SO=0.6, S=0.25, HR=0.15),
         "4"=c(SO=0.9 - p, S=p, HR=0.1),
         "5"=c(SO=0.9 - p, S=p, HR=0.1),
         "6"=c(SO=0.6, S=0.25, HR=0.15),
         "7"=c(SO=0.6, S=0.3, HR=0.1),
         "8"=c(SO=0.6, S=0.3, HR=0.1),
         "9"=c(SO=0.6, S=0.3, HR=0.1))
}

inning_simulation <- function(p){
  outs <- 0
  runs <- 0
  bases = c(FALSE, FALSE, FALSE)
  index <- 1
  
  while (outs < 3 && runs < 3){
    prob <- get_batter_probs(index, p)
    outcome <- sample(c("SO", "S", "HR"), size = 1, prob = prob)
    
    if (outcome == "SO"){
      outs <- outs + 1
    } else if (outcome == "S"){
      adv <- advance_on_single(bases)
      bases <- adv$new_bases
      runs <- adv$runs + runs
    } else if (outcome == "HR"){
      runs <- runs + sum(bases) + 1
      bases <- c(FALSE, FALSE, FALSE)
    }
    if (runs >= 3) break
    index <- index + 1
    if (index > 9) index <- 1
  }
  return(runs) 
}

walkoff_estimation <- function(p, innings = 1000000){
  walkoffs <- replicate(innings, inning_simulation(p))
  mean(walkoffs >= 3)
}

distance <- function(p){
  walkoff_estimation(p) - 0.2
}

solution <- uniroot(distance, lower = 0, upper = 0.9, tol = 0.001)
print(solution$root) #probability is ~0.3811623

p_vals <- seq(0, 0.9, by = 0.1)
walkoff_probs <- sapply(p_vals, walkoff_estimation)

plot_data <- data.frame(
  p = p_vals,
  walkoff_prob = walkoff_probs
)

#graph of p function
ggplot(plot_data, aes(x = p, y = walkoff_prob)) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0.2, color = "red", linetype = "dashed", size = 0.5) +  # target
  geom_vline(xintercept = solution$root, color = "blue", linetype = "dashed", size = 0.5) +  # solution
  geom_point(aes(x = solution$root, y = 0.2), color = "darkgreen", size = 3) + # solution point
  labs(
    title = "Single vs. Walk off Probability Using Monte Carlo Sim",
    x = "p (Single Probability)",
    y = "Estimated Walk-off Probability"
  ) +
  theme_minimal(base_size = 14)



