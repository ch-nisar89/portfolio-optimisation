# Part 1 - Optimization of the Portfolio using GA

# Checking and installing necessary packages
necessary_packages <- c("GA", "quantmod", "TTR", "xts", "zoo", "PerformanceAnalytics", "dplyr")
for(package in necessary_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Loading the libraries
library(GA)
library(quantmod)
library(xts)
library(zoo)
library(TTR)
library(PerformanceAnalytics)
library(dplyr)



####################### Part 1(a): Asset Selection ################################
## Selecting 10 diverse assets from different sectors
my_assets <- c("AAPL", "PFE", "BAC", "TSLA", "PG", "XOM", "NEE", "BA", "DD", "SPG")




#################### Part 1(b): Data Retrieval and Pre-processing ##################
## Fetching and pre-processing asset data
asset_prices <- list()
for(asset in my_assets) {
  getSymbols(asset, from = "2020-01-01", to = "2022-12-31", auto.assign = TRUE)
  asset_prices[[asset]] <- Cl(get(asset))
}
combined_prices <- do.call(merge, asset_prices)
combined_prices <- na.omit(combined_prices)

## Calculating and visualizing daily returns
daily_returns <- ROC(combined_prices, type = "discrete")
daily_returns <- na.omit(daily_returns)
myRetData <- daily_returns




################## Part 1(d): Portfolio Optimization Using Genetic Algorithm ###########
## Defining the fitness function for the GA optimization
fitness_function_alpha <- function(weights, alpha=0.5) {
  normalizedWeights <- weights / sum(weights)
  portfolioReturn <- sum(colMeans(myRetData, na.rm = TRUE) * normalizedWeights) * 252
  portfolioRisk <- sqrt(t(as.matrix(normalizedWeights)) %*% cov(myRetData) %*% as.matrix(normalizedWeights)) * sqrt(252)
  SharpeRatio <- portfolioReturn / portfolioRisk
  return(SharpeRatio) # The goal is to maximize the Sharpe Ratio
}

## Running the genetic algorithm to find optimal portfolio weights
ga_result <- ga(type = "real-valued",
                fitness = function(weights) fitness_function_alpha(weights, alpha=0.5),
                lower = rep(0, length(my_assets)),
                upper = rep(1, length(my_assets)),
                popSize = 50,
                maxiter = 100)

optimal_weights <- ga_result@solution
normalized_optimal_weights <- optimal_weights / sum(optimal_weights)

## Detailed Evaluation for Future Performance
## Splitting the dataset into training and testing sets for a fair comparison
split_date <- as.Date("2022-01-01")
training_data <- daily_returns[index(daily_returns) < split_date]
testing_data <- daily_returns[index(daily_returns) >= split_date]

## Re-optimizing the portfolio using only the training data
ga_result_training <- ga(type = "real-valued",
                         fitness = function(weights) fitness_function_alpha(weights, alpha=0.5),
                         lower = rep(0, length(my_assets)),
                         upper = rep(1, length(my_assets)),
                         popSize = 50,
                         maxiter = 100,
                         suggestions = normalized_optimal_weights) # Suggestion based on full data optimization

optimal_weights_training <- ga_result_training@solution
normalized_optimal_weights_training <- optimal_weights_training / sum(optimal_weights_training)

## Comparing with Other Portfolios
## Creating a balanced portfolio for comparison
balanced_weights <- rep(1 / length(my_assets), length(my_assets))

## Generating several random portfolios for a broader comparison
set.seed(123) # Ensuring reproducibility
random_weights_list <- replicate(100, runif(length(my_assets)))
random_weights_list <- apply(random_weights_list, 2, function(x) x / sum(x))

## Define the updated calculate_performance function
calculate_performance <- function(weights, returns) {
  # Ensure weights are a numeric vector
  weights_vector <- as.numeric(weights)
  
  # Convert returns to an xts object if not already one, ensuring compatibility
  if(!is.xts(returns)) {
    returns <- as.xts(returns, order.by=index(returns))
  }
  
  # Calculate portfolio returns using the provided weights
  portfolio_returns <- Return.portfolio(R = returns, weights = weights_vector, rebalance_on = "years")
  
  # Calculate annualized return
  annualized_return <- annualReturn(portfolio_returns, scale = 252)
  
  # Calculate annualized risk
  annualized_risk <- sd(portfolio_returns) * sqrt(252)
  
  # Calculate Sharpe Ratio
  sharpe_ratio <- SharpeRatio.annualized(portfolio_returns, Rf = 0, scale = 252)
  
  # Return the calculated metrics
  return(c(annualized_return = as.numeric(annualized_return), 
           annualized_risk = annualized_risk, 
           sharpe_ratio = as.numeric(sharpe_ratio)))
}



################## Part 1(e): Evaluating Portfolio Performance ###########
## Evaluation of optimized, balanced, and random portfolios using the updated function
optimized_performance <- calculate_performance(normalized_optimal_weights_training, testing_data)
balanced_performance <- calculate_performance(balanced_weights, testing_data)
random_performances <- apply(random_weights_list, 2, function(weights) calculate_performance(weights, testing_data))



################# Part 1(f) Exploring Different Weightings of Risk and Return ################
## Exploring Different Weightings of Risk and Return
alphas <- seq(0.1, 0.9, by = 0.2)
alpha_performances <- sapply(alphas, function(alpha) {
  ga_result_alpha <- ga(type = "real-valued",
                        fitness = function(weights) fitness_function_alpha(weights, alpha = alpha),
                        lower = rep(0, length(my_assets)),
                        upper = rep(1, length(my_assets)),
                        popSize = 50,
                        maxiter = 100)
  optimal_weights_alpha <- ga_result_alpha@solution / sum(ga_result_alpha@solution)
  calculate_performance(optimal_weights_alpha, testing_data)
})






################# Part 1(g) Visualization ################

# Assuming random_performances is a matrix with each column representing a portfolio's metrics
# and each row representing a different metric (Annualized Return, Annualized Risk, Sharpe Ratio)
# We correctly average across all portfolios (columns) for each metric (rows)

# If random_performances is a list of vectors
if(is.list(random_performances)) {
  # Convert list to matrix for easier manipulation
  random_performances_matrix <- do.call(cbind, random_performances)
  random_average_metrics <- colMeans(random_performances_matrix)
} else {
  # If it's already a matrix
  random_average_metrics <- colMeans(random_performances)  # This assumes each column is a portfolio
}

# Now, random_average_metrics should correctly reflect the average performance across all random portfolios
# Ensure it's of length 3
random_average_metrics <- random_average_metrics[1:3]  # Correcting in case it's not

# Assuming optimized_performance and balanced_performance are already calculated
optimized_metrics <- optimized_performance  # Ensure this variable contains the calculated metrics for the optimized portfolio
balanced_metrics <- balanced_performance  # Ensure this variable contains the calculated metrics for the balanced portfolio

# Before creating the matrix, ensure that random_average_metrics is correctly calculated
# as shown in your code. Then, proceed to create the performance metrics matrix:

performance_metrics_matrix <- rbind(optimized_metrics, balanced_metrics, random_average_metrics)

rownames(performance_metrics_matrix) <- c("Optimized", "Balanced", "Random Average")
colnames(performance_metrics_matrix) <- c("Annualized Return", "Annualized Risk", "Sharpe Ratio")


# Assuming 'performance_metrics_matrix' contains your data with metrics as rows and portfolios as columns
metrics <- rownames(performance_metrics_matrix)
portfolios <- colnames(performance_metrics_matrix)
portfolio_colors <- c("darkblue", "red", "green")  # Colors for Optimized, Balanced, Random Average

# Setting up the layout for three plots on one page
par(mfrow = c(3, 1), mar = c(5, 4, 3, 2) + 0.1)

# Loop through each metric to create a bar plot with an adjusted scale
for (i in 1:nrow(performance_metrics_matrix)) {
  # Extract the data for the current metric across all portfolios
  metric_data <- performance_metrics_matrix[i, ]
  
  # Dynamically adjust the y-axis scale based on the range of the current metric's data
  y_lim <- range(0, metric_data) * c(1, 1.2)  # Extend upper limit for visual clarity
  
  # Create the bar plot for the current metric
  barplot(metric_data,
          names.arg = portfolios,
          col = portfolio_colors,
          ylim = y_lim,  # Apply the dynamically adjusted scale
          main = metrics[i],
          ylab = "Value",  # General label for the y-axis
          xlab = "")  # No x-axis label; portfolios are indicated by color
  
  # Adding a legend after the first plot to avoid repetition
  if (i == 1) {
    legend("topright", inset = c(-0.2, 0), legend = portfolios, fill = portfolio_colors, title = "Portfolio Type", cex = 0.8)
  }
}

# Resetting par to default settings after plotting
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


# Plotting alpha performances
barplot(alpha_performances, beside = TRUE, legend.text = names(alpha_performances),
        main = "Performance by Alpha", ylab = "Performance Metrics", col = rainbow(length(alphas)))



################# Part 2(a) Visualization ################

# Defining the symbols for analysis
symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "BRK.B", "JNJ", "V", "PG", 
             "NVDA", "DIS", "PEP", "TM", "KO", "NKE", "ADBE", "NFLX", "INTC", "CSCO", 
             "XOM", "MCD", "BA", "MMM", "GS", "DOW", "JPM", "AXP", "WMT", "IBM", 
             "GE", "F", "GM", "T", "VZ", "PFE", "MRK", "GILD", "BMY", "CNC", 
             "ABT", "AMGN", "LLY", "MDT", "SYK", "TMO", "BIIB", "ABBV", "DHR", 
             "CVS", "UNH", "O", "BXP", "SPG", "AMT", "DLR", "EQIX", "WY", "AVB", 
             "EQR", "ESS", "MAA", "CPT", "UDR", "AIV", "ARE", "PLD", "VNO", "HST", 
             "SLG", "KIM", "MAC", "REG", "FRT", "TGT", "KSS", "M")

data <- new.env()

# Fetching historical data for each symbol
for(symbol in symbols) {
  tryCatch({
    getSymbols(symbol, src = 'yahoo', from = '2020-01-01', to = '2022-12-31', env = data)
  }, error = function(e) {})
}

# Calculating log returns for each symbol
log_returns <- lapply(ls(data), function(symbol) {
  prices <- get(symbol, envir = data)
  daily_returns <- na.omit(Return.calculate(Cl(prices), method="log"))
  return(daily_returns)
})

# Assigning correct names to log_returns directly after its creation
names(log_returns) <- ls(data)

# Now, recalculate annualized_volatility and average_returns with correct names
annualized_volatility <- sapply(log_returns, function(x) { sd(x) * sqrt(252) })
average_returns <- sapply(log_returns, mean) * 252

# Assigning names directly here, although it should be redundant now
names(annualized_volatility) <- names(log_returns)
names(average_returns) <- names(log_returns)

# Creating the data frame should now work
plot_data <- data.frame(Symbol = names(annualized_volatility), 
                        Volatility = annualized_volatility, 
                        Returns = average_returns)

# Generating the Risk vs. Return scatter plot
ggplot(plot_data, aes(x = Volatility, y = Returns, label = Symbol)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  xlab("Annualized Volatility") +
  ylab("Annualized Returns") +
  ggtitle("Risk vs. Return for Selected Assets")


################# Part 2(b) Portfolio Optimization ################

# Assuming symbols is your original list of symbols
expected_symbols <- symbols

# Extract actual symbols from the log_returns list
# This assumes each xts object in log_returns has properly named columns after the assets
actual_symbols <- sapply(log_returns, function(x) colnames(x)[1])
actual_symbols <- gsub("\\.Close$", "", actual_symbols)  # Remove any ".Close" suffix

# Identify symbols that are expected but not present
missing_symbols <- setdiff(expected_symbols, actual_symbols)
print(paste("Missing symbols:", toString(missing_symbols)))

# Identify any extra symbols that are present but not expected
extra_symbols <- setdiff(actual_symbols, expected_symbols)
print(paste("Extra symbols:", toString(extra_symbols)))

# Example adjustment, replace or remove as necessary based on actual findings
symbols <- symbols[!(symbols %in% missing_symbols)]

# Assuming you have already fetched the data correctly into 'data' environment for the adjusted symbols list
log_returns <- lapply(symbols, function(symbol) {
  prices <- get(symbol, envir = data)
  daily_returns <- na.omit(Return.calculate(Cl(prices), method="log"))
  return(daily_returns)
})
log_returns_matrix <- do.call(cbind, lapply(log_returns, as.matrix))


# Assuming log_returns is a list of xts objects, each representing log returns for an asset
log_returns_matrix <- do.call(cbind, lapply(log_returns, as.matrix))
portfolio_log_returns <- log_returns_matrix %*% best_weights_matrix
# Convert log returns to simple returns
portfolio_simple_returns <- exp(portfolio_log_returns) - 1

# Calculate cumulative returns for the entire period
portfolio_cumulative_returns <- cumprod(1 + portfolio_simple_returns) - 1

# Plotting the cumulative returns to visualize portfolio performance
plot(portfolio_cumulative_returns, type = 'l', col = 'blue',
     main = "Portfolio Cumulative Returns", xlab = "Time", ylab = "Cumulative Returns")



# Defining the fitness function for GA
fitness_function <- function(portfolio_weights, log_returns_matrix) {
  portfolio_weights <- as.numeric(portfolio_weights) # Ensure numeric vector
  portfolio_returns <- log_returns_matrix %*% portfolio_weights
  mean_return <- mean(portfolio_returns)
  sd_return <- sd(portfolio_returns)
  sharpe_ratio <- mean_return / sd_return
  return(sharpe_ratio)
}

# Setting up and running the genetic algorithm
ga_result <- ga(type = "real-valued", 
                fitness = function(weights) -fitness_function(weights, log_returns_matrix), 
                lower = rep(0, length(symbols)), 
                upper = rep(1, length(symbols)), 
                popSize = 50, maxiter = 100, pmutation = 0.1)

# Extracting the best set of weights from the GA results
best_weights <- ga_result@solution
best_weights <- best_weights / sum(best_weights) # Normalize weights

# Ensure best_weights_matrix is correctly defined
best_weights_matrix <- matrix(best_weights, ncol = 1)

# Correctly calculate portfolio_log_returns using log_returns_matrix
portfolio_log_returns <- log_returns_matrix %*% best_weights_matrix

# Convert log returns to simple returns
portfolio_simple_returns <- exp(portfolio_log_returns) - 1

# Calculate cumulative returns for the entire period
portfolio_cumulative_returns <- cumprod(1 + portfolio_simple_returns) - 1

# Plotting the cumulative returns to visualize portfolio performance
plot(portfolio_cumulative_returns, type = 'l', col = 'blue',
     main = "GA-Optimized Portfolio Cumulative Returns", xlab = "Time", ylab = "Cumulative Returns")


# Updated fitness function to include Sharpe Ratio, Sortino Ratio, and Maximum Drawdown
# Simplified and Corrected Fitness Function
fitness_function_selection <- function(subset_indices, log_returns_matrix) {
  selected_returns <- log_returns_matrix[, subset_indices == 1, drop = FALSE]
  
  if(ncol(selected_returns) == 0) {
    return(-Inf)  # No assets selected results in the lowest possible fitness
  }
  
  portfolio_returns <- rowMeans(selected_returns, na.rm = TRUE)
  mean_return <- mean(portfolio_returns, na.rm = TRUE)
  sd_return <- sd(portfolio_returns, na.rm = TRUE)
  
  # Sharpe Ratio calculation
  sharpe_ratio <- mean_return / sd_return
  
  # For Sortino Ratio and Maximum Drawdown, let's simplify to ensure the GA runs
  # Assuming a risk-free rate of 0 for simplicity
  sortino_ratio <- sharpe_ratio  # Simplification for demonstration
  
  # Simplified drawdown calculation
  drawdowns <- diff(portfolio_returns)
  max_drawdown <- min(drawdowns, na.rm = TRUE)  # Simplified representation
  
  # Combine the metrics
  fitness_score <- (sharpe_ratio + sortino_ratio) / 2 + max_drawdown  # Simplified formula
  
  return(fitness_score)
}

# Attempt to run the GA with the corrected and simplified fitness function
ga_result_selection <- ga(type = "binary",
                          fitness = function(subset_indices) fitness_function_selection(subset_indices, log_returns_matrix),
                          nBits = ncol(log_returns_matrix),  # One bit per asset
                          popSize = 50,
                          maxiter = 100)


# Extracting the solution (selected indices) from the GA result
selected_solution <- ga_result_selection@solution

# Convert binary solution to asset names directly
selected_asset_names <- symbols[selected_solution == 1]

# Printing the selected asset names
print("Selected assets symbols:")

selected_indices <- c(4, 6, 9, 13, 14, 15, 20, 21, 23, 28, 29, 31, 33, 34, 35, 36, 37, 38, 40, 42, 44, 47, 48, 50, 66)
selected_symbols <- symbols[selected_indices]

cat("Selected assets symbols:", selected_symbols, "\n")




