# Load the necessary package
library(ggplot2)
library(dplyr)
# Set a seed for reproducibility
set.seed(123)
# Number of data points
num_points <- 1000
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exponential Distribution
# Generating data from exponential distribution with different rates (λ)
rate_values <- c(5, 2.5, 0.1)
exp_data <- lapply(rate_values, function(rate) {
rexp(n = 1000, rate = rate)
})
# Creating probability plots for different rate values
exp_plots <- lapply(1:length(rate_values), function(i) {
p <- ggplot(data.frame(x = exp_data[[i]]), aes(x)) +
geom_histogram(binwidth = 0.5, fill = "plum2", color = "black", alpha = 0.7) +
geom_density(kernel = "gaussian", color = "red") +
labs(title = paste("Exponential Distribution (λ =", rate_values[i], ")"),
x = "Value", y = "Density") +
theme_minimal()
return(p)
})
# Displaying probability plots
print(exp_plots[[1]])
print(exp_plots[[2]])
print(exp_plots[[3]])
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Gamma Distribution
# Generating data from gamma distribution with varying shape and scale parameters
# Varying mean and standard deviation parameters
shape_values <- c(1, 2, 3)
scale_values <- c(0.5, 1, 1.5)
for (shape_val in shape_values) {
for (scale_val in scale_values) {
# Generating gamma random data
gamma_data <- rgamma(num_points, shape = shape_val, scale = scale_val)
# Preparing the plot's canvas.
plot_data <- data.frame(x = gamma_data)
# Creating a histogram
plot <- ggplot(plot_data, aes(x)) +
geom_histogram(binwidth = 0.5, fill = "olivedrab3", color = "black", alpha = 0.7) +
labs(title = paste("Gamma Distribution - Shape:", shape_val, "Scale:", scale_val),
x = "Value", y = "Frequency") +
theme_minimal()
# Displaying the histogram
print(plot)
}
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Lognormal Distribution
# Varying mean and standard deviation parameters
mean_values <- c(0, 1, 2)
sd_values <- c(0.5, 1, 1.5)
num_points <- 1000 # Number of data points
# Generating lognormal random data and creating histograms
for (mean_val in mean_values) {
for (sd_val in sd_values) {
# Generating lognormal random data
lognormal_data <- rlnorm(num_points, meanlog = mean_val, sdlog = sd_val)
# Preparing the plot's canvas.
plot_data <- data.frame(x = lognormal_data)
# Creating a histogram using ggplot2
plot <- ggplot(plot_data, aes(x)) +
geom_histogram(binwidth = 0.5, fill = "paleturquoise2", color = "black", alpha = 0.7) +
labs(title = paste("Lognormal Distribution - Mean:", mean_val, "SD:", sd_val),
x = "Value", y = "Frequency") +
theme_minimal()
# Displaying the histogram
print(plot)
}
}
