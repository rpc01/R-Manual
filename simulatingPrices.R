####
# SIMULATE LOG NORMAL PRICES
####
library(quantmod)
library(dplyr)
from<-"2018-01-01"
till<-"2020-04-09"
getSymbols("F",src="yahoo",from=from, to=till)%>%get() # from yahoo 

MA_log_returns <-F %>%
  dailyReturn(type="log")
head(MA_log_returns)  

names(MA_log_returns) <- "MA.Log.Returns"

# Plot the log-returns
library(ggplot2)
MA_log_returns %>%    
  ggplot(aes(x = MA.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5)

# Examine the probability distribution
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- MA_log_returns %>% 
  quantile(probs = probs, na.rm = TRUE)
dist_log_returns

mean_log_returns <- mean(MA_log_returns, na.rm = TRUE)
sd_log_returns <- sd(MA_log_returns, na.rm = TRUE)

mean_log_returns %>% exp()

head(MA_log_returns)
tail(MA_log_returns)

#Random Walk
#remove.packages("dygraphs")
# Parameters
N     <- 1000
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <- F$F.Adjusted[[nrow(F$F.Adjusted)]]
# Simulate prices
set.seed(386) 
price  <- c(price_init, rep(NA, N-1))
for(i in 2:N) {
  price[i] <- price[i-1] * exp(rnorm(1, mu, sigma))
}
price_sim <- cbind(day, price) %>% 
  as_tibble()
# Visualize price simulation
price_sim %>%
  ggplot(aes(day, price)) +
  geom_line() +
  ggtitle(paste0("MA: Simulated Prices for ", N," Trading Days"))

# Montecarlo Simulation
library(timetk)
remove.packages("tidyquant")
library(tidyr)
N     <- 252 # Number of Stock Price Simulations
M     <- 25  # Number of Monte Carlo Simulations   
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <- F$F.Adjusted[[nrow(F$F.Adjusted)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)

monte_carlo_mat[1, ] <- price_init

for (j in 1:M) {
  
  for(i in 2:N) {
    monte_carlo_mat[i, j] <- monte_carlo_mat[i - 1, j] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>%
  timetk::tk_tbl()

#nm <- str_c("Sim.", seq(1, M))
#works similarly to paste0 below
nm <- paste0("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(paste0("MA: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))
dim(price_sim)
tail(price_sim)
dim(monte_carlo_mat)

