# Homework (4): Permutation Test
# Diana Paola Ayala Roldan (A01365809)
# Jose Ruben Villicaña Ibargüengoytia (A01654347)
# Carlos Cristobal Rubio Hernandez (A01745245)

# Clear all
graphics.off() # Clean plots
rm(list = ls()) # Clean global environment
cat("\014") # Clean console

# Load libraries
library(ggplot2)
library(gapminder)
library(perm)
library(MKinfer)

data("gapminder")

# Data from Africa and Europe in year 2007
data_africa <- gapminder[gapminder$continent == "Africa" & gapminder$year == 2007, ]
data_europe <- gapminder[gapminder$continent == "Europe" & gapminder$year == 2007, ]

# T-Test
t.test(data_africa$gdpPercap, data_europe$gdpPercap, var.equal = TRUE)

# Histogram of GDP data from Africa and Europe
hist(data_europe$gdpPercap,
     col = "blue",
     main = "Histogram: GDP from Africa and Europe (2007)",
     xlab = "GDP [USD]",
     xlim = c(min(data_africa$gdpPercap), max(data_europe$gdpPercap)),
     ylim = c(0, 35))
hist(data_africa$gdpPercap, add = TRUE, col = "red")
legend("topright", legend = c("Europe", "Africa"), fill = c("blue", "red"))

######################################################################
######################### Permutation Test ###########################
######################################################################

data <- subset(gapminder, (continent == "Africa" & year == 2007) | 
                           (continent == "Europe" & year == 2007))

# T-Test of data
sample_t <- t.test(gdpPercap ~ continent, data = data, var.equal = TRUE)$statistic

# Number of permutations
B <- 9999
permutation_t <- vector("numeric", B)

# Progress bar
pbar <- txtProgressBar(min = 0, max = B, style = 3)

# Calculate permutation test
for (i in 1:B) {
  # Randomize data of continent
  data$continent <- sample(data$continent, length(data$continent), replace = FALSE)
  # T-Test with permutation
  permutation_t[i] <- t.test(gdpPercap ~ continent, data = data, var.equal = TRUE)$statistic
  setTxtProgressBar(pbar, i)
}
close(pbar)

# Calculate p-value approximation from permutation test of two tails
p_value_permutation <- mean(abs(permutation_t) > abs(sample_t))

# Histogram of permutation t with sample_t line
hist(permutation_t, xlim = c(sample_t - 1, 5),
     main = "Histogram: Permutation t",
     xlab = "Permutation levels")
abline(v = sample_t, col = "red")

# t-test using MKinfer
perm_t_test_result <- perm.t.test(gdpPercap ~ continent, 
                                  data = subset(gapminder, 
                                                continent == "Africa" & year == 2007 | 
                                                continent == "Europe" & year == 2007))

# Show result perm.t.test
perm_t_test_result
