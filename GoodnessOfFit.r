# https://posit.cloud/content/6409670
# Import the dataset (assuming it's in a CSV file named "bejaia_data.csv")
data <- read_csv("bejaia_data.csv")
# List of columns to analyze
columns_to_analyze <- c("Temperature", "RH", "Ws", "Rain", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI")
# Create an empty list to store the results for each column
results_list <- list()
# Loop through the columns and perform analysis
for (col_name in columns_to_analyze) {
# Extract the data for the current column
column_data <- data[[col_name]]
# Replace zero values with a small positive value
column_data[column_data == 0] <- 0.000000000001
# Remove rows with missing or invalid values
data <- na.omit(data)
# Perform distribution fitting and goodness-of-fit tests for the current column
fit_results <- fitting_dist(column_data)
# Store the results in the results_list with the column name as the key
results_list[[col_name]] <- fit_results
# Print or further analyze the results for the current column
cat("Analysis for Column:", col_name, "\n")
# Access GOF results for each distribution for the current column
for (dist_name in c("normal", "lognormal", "gamma", "exponential")) {
cat(dist_name, "Distribution KS Test Statistic: ", fit_results$ks[[dist_name]]$statistic, "\n")
cat(dist_name, "Distribution KS Test P-value: ", fit_results$ks[[dist_name]]$p.value, "\n")
# cat(dist_name, "Distribution CVM Test Statistic: ", fit_results$cvm[[dist_name]]$statistic, "\n")
# cat(dist_name, "Distribution CVM Test P-value: ", fit_results$cvm[[dist_name]]$p.value, "\n")
# cat(dist_name, "Distribution AD Test Statistic: ", fit_results$ad[[dist_name]]$statistic, "\n")
# cat(dist_name, "Distribution AD Test P-value: ", fit_results$ad[[dist_name]]$p.value, "\n")
}
cat("\n")
}
# Functions
gft <- function(data) {
return(NULL)
}
fitting_dist <- function(data) {
# Fit distributions
fit_normal <- fitdistr(data, "normal")
fit_lognormal <- fitdistr(log(data), "normal")
fit_gamma <- fitdistr(data, "gamma")
fit_exponential <- fitdistr(data, "exponential")
# KS GOF test
7/9/23, 21:09 Posit Cloud
https://posit.cloud/content/6409670 2/2
ks_results <- list(
normal = ks.test(unique(data), "pnorm", mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]),
lognormal = ks.test(unique(data), "plnorm", meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]),
gamma = ks.test(unique(data), "pgamma", shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]),
exponential = ks.test(unique(data), "pexp", rate = fit_exponential$estimate[1])
)
# Cramer-Von Mises
# cvm_results <- list(
# normal = cvm.test(data, pnorm, mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]),
# lognormal = cvm.test(data, plnorm, meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]),
# gamma = cvm.test(data, pgamma, shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]),
# exponential = cvm.test(data, pexp, rate = fit_exponential$estimate[1])
# )
# ad_results <- list()
# Anderson-Darling
# ad_results$normal <- ad.test(data)
# ad_results$lognormal <- ad.test(log(data))
# ad_results$gamma <- ad.test(data)
# ad_results$exponential <- ad.test(data)
results <- list(
fit = list(
normal = fit_normal,
lognormal = fit_lognormal,
gamma = fit_gamma,
exponential = fit_exponential
),
ks = ks_results,
# cvm = cvm_results,
# ad = ad_results
)
return(results)
}
7/9/23, 21:13 Posit Cloud

library(ggplot2)
library(readr) # Load the readr library for read_csv
# Import the dataset (assuming it's in a CSV file named "bejaia_data.csv")
data <- read_csv("bejaia_data.csv")
# Create a bar plot of the 'Classes' column with theme_minimal() and blue color
ggplot(data, aes(x = Classes)) +
geom_bar(fill = "slateblue1") +
labs(title = "Distribution of Classes", x = "Classes", y = "Count") +
theme_minimal()
# Define observed_fire, observed_not_fire, and total_observations
observed_fire <- sum(data$Classes == "fire")
observed_not_fire <- sum(data$Classes == "not fire")
total_observations <- nrow(data)
# Define a vector of distribution names
distribution_names <- c("Poisson", "Binomial", "Negative Binomial", "Geometric", "Hypergeometric", "Discrete
Uniform")
# Iterate through the distribution names
for (dist_name in distribution_names) {
# Calculate the expected frequencies and perform the Chi-squared test
if (dist_name == "Poisson") {
lambda <- observed_fire # Use observed frequency for Poisson
expected_fire <- dpois(0:total_observations, lambda) * total_observations
expected_not_fire <- total_observations - expected_fire
} else if (dist_name == "Binomial") {
p <- sum(data$Classes == "fire") / total_observations
n <- total_observations
expected_fire <- dbinom(0:total_observations, size = n, prob = p) * n
expected_not_fire <- total_observations - expected_fire
} else if (dist_name == "Negative Binomial") {
p <- sum(data$Classes == "fire") / total_observations
r <- observed_fire # Use observed frequency for Negative Binomial
expected_fire <- dnbinom(0:total_observations, size = r, prob = p) * total_observations
expected_not_fire <- total_observations - expected_fire
} else if (dist_name == "Geometric") {
p <- sum(data$Classes == "fire") / total_observations
expected_fire <- dgeom(0:total_observations, prob = p) * total_observations
expected_not_fire <- total_observations - expected_fire
} else if (dist_name == "Hypergeometric") {
N <- total_observations
K <- observed_fire
n <- 10 # You can adjust this to your desired sample size
expected_fire_sample <- dhyper(0:n, m = K, n = N - K, k = n) * n
expected_fire <- sum(expected_fire_sample)
expected_not_fire <- total_observations - expected_fire
} else if (dist_name == "Discrete Uniform") {
# For Discrete Uniform, assume equal probabilities for "fire" and "not fire"
expected_fire <- rep(observed_fire / total_observations, total_observations)
expected_not_fire <- rep(observed_not_fire / total_observations, total_observations)
}
7/9/23, 21:13 Posit Cloud
https://posit.cloud/content/6409670 2/2
# Create a contingency table
contingency_table <- matrix(c(observed_fire, observed_not_fire, expected_fire, expected_not_fire), nrow = 2)
# Perform the Chi-squared test
chi_squared_result <- chisq.test(contingency_table)
# Print the results for the current distribution
cat(dist_name, "Distribution X-squared =", chi_squared_result$statistic, "\n")
cat(dist_name, "Distribution p-value =", chi_squared_result$p.value, "\n\n")
}
--------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
# Import the dataset (assuming it's in a CSV file named "bejaia_data.csv")
data <- read.csv("bejaia_data.csv")
# Encode 'Classes' column as numerical values
data$Classes <- ifelse(data$Classes == "not fire", 1, 2)
# List of columns to analyze
columns_to_analyze <- c("Temperature", "RH", "Ws", "Rain", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "Classes")
# Define a simple plot_data function
plot_data <- function(data, col_name) {
hist_plot <- ggplot(data.frame(x = data), aes(x = x)) +
geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
geom_density(fill = "red", alpha = 0.5) +
labs(title = paste("Distribution of", col_name),
x = col_name,
y = "Density") + # Add axis labels
scale_fill_manual(values = c("blue" = "blue", "red" = "red")) + # Specify legend colors
theme_minimal()
print(hist_plot)
}
# Create an empty list to store the results for each column
results_list <- list()
# Loop through the columns and perform analysis
for (col_name in columns_to_analyze) {
# Extract the data for the current column
column_data <- data[[col_name]]
# Replace zero values with a small positive value
column_data[column_data == 0] <- 0.000000000001
# Plot the data
plot_data(column_data, col_name)
}
