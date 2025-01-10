# Homework (5): Hotelling T2 and Power Analysis
# Diana Paola Ayala Roldan (A01365809)
# Jose Ruben Villicana Ibarguengoytia (A01654347)
# Carlos Cristobal Rubio Hernandez (A01745245)

# Clear all
graphics.off() # Clean plots
rm(list = ls()) # Clean global environment
cat("\014") # Clean console
set.seed(242)

# Libraries
library(corrplot)
library(ggplot2)
library(mvnormtest)
library(Hotelling)
library(pwr)

# Functions
histogram <- function(data1, data2, title, legend1, legend2) {
  x_max <- max(c(max(data1), max(data2)))
  x_min <- min(c(min(data1), min(data2)))
  y_max <- max(c(max(hist(data1, plot = FALSE)$counts),
                 max(hist(data2, plot = FALSE)$counts)))
  hist(data1, col = "lightblue", main = title, xlab = "Values", ylab = "Frequency",
       border = "black", xlim = c(x_min, x_max), ylim = c(0, y_max))
  hist(data2, add = TRUE, col = "lightgreen", border = "black")
  legend("topright", legend = c(legend1, legend2), fill = c("lightblue", "lightgreen"))
}

create_scatterplot <- function(dataset, title) {
  p <- ggplot(dataset, aes(x = quality, y = .data[[variable]])) +
    geom_point(col = "blue") +
    labs(x = "\nQuality\n", y = variable) +
    ggtitle(title) +
    geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1))
  return(p)
}

create_boxplot <- function(dataset, title, label) {
  boxplot(dataset[[variable]] ~ quality, data = dataset, main = title,
          ylab = label, xlab = "Ranked Quality")
}

hot.plot <- function(x, title, ...) {
  if (is.na(match("results", names(x)))) {
    stop("Plotting only works if you have used the permutation test")
  }
  dotArgNames = names(list(...))
  if ("xlim" %in% dotArgNames) {
    with(x, {
      h <- hist(stats$m * results, main = title,
                xlab = expression(T^2), ...)
      T0 <- with(stats, m * statistic)
      lines(rep(T0, 2), c(0, max(h$counts)), lwd = 2)
    })
  } else {
    with(x, {
      r <- range(c(stats$m * results, stats$m * stats$statistic))
      pr <- pretty(r)
      h <- hist(stats$m * results, main = title,
                xlab = expression(T^2), xlim = c(min(pr), max(pr)), ...)
      T0 <- with(stats, m * statistic)
      lines(rep(T0, 2), c(0, max(h$counts)), lwd = 2)
    })
  }
}

perform_hotelling_test_and_power <- function(tit, data, variable_to_compare) {
  t2 <- hotelling.test(
    data[[variable_to_compare[[1]]]] +
      data[[variable_to_compare[[2]]]] +
      data[[variable_to_compare[[3]]]] ~ quality, data = data
  )
  t2p <- hotelling.test(
    data[[variable_to_compare[[1]]]] +
      data[[variable_to_compare[[2]]]] +
      data[[variable_to_compare[[3]]]] ~ quality, data = data, perm = TRUE
  )
  hot.plot(t2p, tit)
  curr_power <- pwr.t.test(
    n = c(sum(data$quality == 5, na.rm = TRUE),
          sum(data$quality == 6, na.rm = TRUE)),
    d = cohen.ES(test = "t", size = "medium")$effect.size,
    sig.level = 0.05, type = "two.sample"
  )
  cat("\nHotelling's T^2 test assuming normality:\nP-Value:", t2$pval)
  cat("\n\nHotelling's T^2 with permutation:\nP-Value:", t2p$pval)
  cat("\n\nThe power of the Hotelling T2 Test is:", curr_power$power, "\n")
  return(t2)
}

################################################################################
########################### Task 1: Understanding the Data ######################
################################################################################
winequality_red <- read.csv(
  "D:/Users/stccr/Desktop/Trabajos/M 2do/Data Science and Inference/Homeworks/Hotelling T2/winequality-red.csv",
  sep = ";"
)

winesubset <- subset(winequality_red, quality == 5 | quality == 6)
q6 <- subset(winequality_red, quality == 6)
q5 <- subset(winequality_red, quality == 5)

# Description
summary(winequality_red)

# Number of variables
number_of_variables_red <- ncol(winequality_red)

# Number of observations
number_of_observations_red <- nrow(winequality_red)

# Graphical Illustration: Histogram
histogram(q5$fixed.acidity, q6$fixed.acidity, "Red Wine (5) Acidity vs Red Wine (6) Acidity",
          "Acidity 5", "Acidity 6")
histogram(q5$residual.sugar, q6$residual.sugar, "Red Wine (5) Sugar vs Red Wine (6) Sugar",
          "Sugar (5)", "Sugar (6)")
histogram(q5$pH, q6$pH, "Red Wine (5) pH vs Red Wine (6) pH",
          "pH (5)", "pH (6)")

# Graphical Illustration: Scatterplots
variables_to_visualize <- c("pH", "fixed.acidity", "residual.sugar")
for (variable in variables_to_visualize) {
  plot_title <- paste("Red Wine: Quality vs.", variable)
  p <- create_scatterplot(winequality_red, plot_title)
  print(p)
}

################################################################################
######################## Task 2 & 3: Hypothesis Testing #########################
################################################################################
# Select the specific variables you want to compare
vars_to_compare <- c("pH", "fixed.acidity", "residual.sugar")

# Perform Hotelling's T^2 test and power analysis on red wine data
cat("Analysis for Red Wine:\n")
tit <- "Distribution of permuted test stats (original Red Wine sample)"
perform_hotelling_test_and_power(tit, winesubset, vars_to_compare)

# Calculate the required sample size for a specified power
effect_size <- cohen.ES(test = "t", size = "medium")$effect.size
alpha <- 0.05
power <- 0.8
required_sample_size <- pwr.t.test(d = effect_size, sig.level = alpha,
                                    power = power, type = "two.sample")
cat(paste("\n\nRequired sample size for power =", power, "and alpha =", alpha, ":"))
print(required_sample_size)

# Plot the power analysis
plot(required_sample_size)

# Create subset with fewer observations and compute Hotelling Test & Power for Red Wine Data
n <- c(30, 40, 50, 60, 70)
for (i in n) {
  wine_reduced <- rbind(
    q6[sample(1:nrow(q6), i, replace = FALSE), ],
    q5[sample(1:nrow(q5), i, replace = FALSE), ]
  )
  cat("\n\n\nFor a sample size of:", i, "\n")
  tit <- paste("Distribution of permuted test stats (", i, " sample size)")
  perform_hotelling_test_and_power(tit, wine_reduced, vars_to_compare)
}
