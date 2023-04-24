library(quantmod)
library(TTR)
install.packages('ragtop')
library(ragtop)
library(reticulate)
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
install.packages('rjson')
library(rjson)

# Define ticker symbols for the index funds
tickers <- c("FSTA","FDIS","FENY","FIDU","FMAT","FUTY","FTEC","FCOM","FNCL","FHLC","FREL")

# Download data for the tickers
getSymbols(tickers)

# Extract the closing prices for each ticker
prices <- list()
for (ticker in tickers) {
  prices[[ticker]] <- Cl(get(ticker))
}

# Print the current prices for each ticker
for (ticker in tickers) {
  current_price <- prices[[ticker]][nrow(prices[[ticker]])]
  print(paste0("The current price of ", ticker, " is $", round(current_price, 2)))
}

current_prices <- list()
for (ticker in tickers) {
  current_prices[[ticker]] <- current_price
}

# Calculate volatility for each ticker using Monte Carlo Simulation
volatilities <- c(fromJSON(file = "file:///Users/zachfindling/Documents/json_variances.json"))
print(volatilities)
# Print the volatility for each ticker
for (ticker in tickers) {
  print(paste0("The volatility of ", ticker, " is ", volatilities[[ticker]]))
}

# Define parameters for Black-Scholes pricing
for (ticker in tickers) {
  S <- prices[[ticker]][nrow(prices[[ticker]])] # underlying asset price (current price as of 4/18/2023)
  K <- n1 # strike price
  r <- 0.0358 # risk-free interest rate
  t <- n2/365 # time to maturity (in years)
  sigma <- as.numeric(volatilities[ticker])
  # Price a European call option that matures in one year if the current stock price is 80
  bslist <- list()
  bslist[[ticker]] <- blackscholes(1, S, K, r, t, sigma, 0, 0, 0, NULL)
  print(bslist[[ticker]])
}
