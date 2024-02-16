# Part 1 - Optimization of the Portfolio using GA

# Checking and installing necessary packages
necessary_packages <- c("GA", "quantmod", "TTR", "xts", "zoo", "PerformanceAnalytics")
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

# Recreate the performance metrics matrix
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

