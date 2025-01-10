# Load necessary libraries
library(fitdistrplus)
library(ggplot2)
# Set the random seed for reproducibility
set.seed(123)
# Step 1: Generate random samples from an exponential distribution
n <- 1000
rate <- 0.2 # Desired rate parameter for exponential distribution
exp_samples1 <- rexp(n, rate)
exp_samples2 <- rexp(n, rate)
# Step 2: Sum the generated exponential samples
sum_exponential <- exp_samples1 + exp_samples2
# Step 3: Estimate parameters of gamma distribution
fit_gamma <- fitdist(sum_exponential, "gamma") # Use fitdist instead of
fitdistr
shape_gamma <- fit_gamma$estimate["shape"]
rate_gamma <- fit_gamma$estimate["rate"]
scale_gamma <- 1 / rate_gamma # Calculate scale parameter for gamma
distribution
# Step 4: Plot histograms of simulated gamma data and sum of exponential with
lowered opacity
simulated_gamma <- rgamma(n, shape = shape_gamma, rate = rate_gamma) #
Generate simulated data from gamma distribution
hist(sum_exponential, breaks = 30, freq = FALSE, col = alpha("yellow", 0.5),
main = "Histogram of Simulated Gamma Data and Sum of Exponential")
hist(simulated_gamma, breaks = 30, freq = FALSE, col = alpha("lightblue",
0.5), add = TRUE)
legend("topright", legend = c("Sum Exponential", "Simulated Gamma"), col =
c(alpha("yellow", 0.5), alpha("lightblue", 0.5)), lwd = 2)
# Step 5: Compute mean and variance of both distributions
exp_mean1 <- mean(exp_samples1)
exp_variance1 <- var(exp_samples1)
exp_mean2 <- mean(exp_samples2)
exp_variance2 <- var(exp_samples2)
gamma_mean <- shape_gamma * scale_gamma # Use scale parameter for
calculating mean
gamma_variance <- shape_gamma * scale_gamma^2 # Use scale parameter for
calculating variance
# Print results
cat("Exponential Sample 1:\n")
cat(" Mean:", exp_mean1, "\n")
cat(" Variance:", exp_variance1, "\n\n")
cat("Exponential Sample 2:\n")
cat(" Mean:", exp_mean2, "\n")
cat(" Variance:", exp_variance2, "\n\n")
cat("Fitted Gamma distribution:\n")
cat(" Mean:", gamma_mean, "\n")
cat(" Variance:", gamma_variance, "\n")
# Step 6: Create theoretical and empirical CDF plots
x_vals <- seq(0, max(sum_exponential, simulated_gamma), length.out = 100)
empirical_cdf_exp <- ecdf(sum_exponential)(x_vals)
empirical_cdf_gamma <- ecdf(simulated_gamma)(x_vals)
theoretical_cdf_gamma <- pgamma(x_vals, shape = shape_gamma, rate =
rate_gamma)
cdf_data <- data.frame(x = x_vals, Empirical_CDF_Exp = empirical_cdf_exp,
Empirical_CDF_Gamma = empirical_cdf_gamma, Theoretical_CDF_Gamma =
theoretical_cdf_gamma)
ggplot(cdf_data, aes(x)) +
geom_step(aes(y = Empirical_CDF_Exp), color = "blue", linetype = "dashed")
+
geom_step(aes(y = Empirical_CDF_Gamma), color = "green", linetype =
"dashed") +
geom_line(aes(y = Theoretical_CDF_Gamma), color = "red") +
labs(title = "Empirical vs Theoretical CDF (Exponential vs Gamma)", y =
"Probability", x = "Value") +
theme_minimal()
# Step 7: Create theoretical and empirical PDF plots
pdf_data_gamma <- data.frame(x = x_vals, Theoretical_PDF_Gamma =
dgamma(x_vals, shape = shape_gamma, rate = rate_gamma))
empirical_pdf_exp <- density(sum_exponential)
pdf_data_exp <- data.frame(x = empirical_pdf_exp$x, Empirical_PDF_Exp =
empirical_pdf_exp$y)
pdf_data_exp$Theoretical_PDF_Gamma <- dgamma(pdf_data_exp$x, shape =
shape_gamma, rate = rate_gamma)
ggplot(pdf_data_exp, aes(x)) +
geom_line(aes(y = Empirical_PDF_Exp), color = "blue", linetype = "dashed")
+
geom_line(aes(y = Theoretical_PDF_Gamma), color = "red") +
labs(title = "Empirical vs Theoretical PDF (Exponential vs Gamma)", y =
"Density", x = "Value") +
theme_minimal()
# Load necessary libraries
library(fitdistrplus)
library(ggplot2)
# Set the random seed for reproducibility
set.seed(123)
# Step 1: Generate random samples from a gamma distribution
n <- 1000
shape_gamma <- 2 # Desired shape parameter for gamma distribution
scale_gamma <- 1
gamma_samples1 <- rgamma(n, shape_gamma, scale = scale_gamma)
gamma_samples2 <- rgamma(n, shape_gamma, scale = scale_gamma)
# Step 2: Estimate parameters of beta distribution
alpha_beta <- shape_gamma
beta_beta <- 2 * scale_gamma
# Step 3: Plot histograms of gamma and beta distributions
hist(gamma_samples1, breaks = 30, freq = FALSE, col = rgb(0, 0, 1, 0.3),
border = "blue", main = "Histogram of Gamma and Beta Distributions")
hist(gamma_samples2, breaks = 30, freq = FALSE, col = rgb(0, 1, 0, 0.3),
border = "green", add = TRUE)
hist(rgamma(n, shape_gamma, scale = scale_gamma), breaks = 30, freq = FALSE,
col = rgb(1, 0, 0, 0.3), border = "red", add = TRUE)
legend("topright", legend = c("Gamma Sample 1", "Gamma Sample 2", "Beta
Distribution"),
col = c(rgb(0, 0, 1, 0.3), rgb(0, 1, 0, 0.3), rgb(1, 0, 0, 0.3)), lwd
= 2)
# Step 4: Compute mean and variance of all distributions
gamma_mean1 <- mean(gamma_samples1)
gamma_variance1 <- var(gamma_samples1)
gamma_mean2 <- mean(gamma_samples2)
gamma_variance2 <- var(gamma_samples2)
beta_mean <- alpha_beta / (alpha_beta + beta_beta)
beta_variance <- (alpha_beta * beta_beta) / ((alpha_beta + beta_beta)^2 *
(alpha_beta + beta_beta + 1))
# Print results
cat("Gamma Sample 1:\n")
cat(" Mean:", gamma_mean1, "\n")
cat(" Variance:", gamma_variance1, "\n\n")
cat("Gamma Sample 2:\n")
cat(" Mean:", gamma_mean2, "\n")
cat(" Variance:", gamma_variance2, "\n\n")
cat("Beta Distribution:\n")
cat(" Mean:", beta_mean, "\n")
cat(" Variance:", beta_variance, "\n")
# Step 5: Create theoretical and empirical CDF plots
x_vals <- seq(0, max(gamma_samples1, gamma_samples2), length.out = 100)
empirical_cdf1 <- ecdf(gamma_samples1)(x_vals)
empirical_cdf2 <- ecdf(gamma_samples2)(x_vals)
theoretical_cdf <- pgamma(x_vals, shape_gamma, scale = scale_gamma)
cdf_data <- data.frame(x = x_vals, Empirical_CDF1 = empirical_cdf1,
Empirical_CDF2 = empirical_cdf2, Theoretical_CDF = theoretical_cdf)
ggplot(cdf_data, aes(x)) +
geom_step(aes(y = Empirical_CDF1), color = "blue", linetype = "dashed") +
geom_step(aes(y = Empirical_CDF2), color = "green", linetype = "dashed") +
geom_line(aes(y = Theoretical_CDF), color = "red") +
labs(title = "Empirical vs Theoretical CDF", y = "Probability", x =
"Value") +
theme_minimal()
# Step 6: Create theoretical and empirical PDF plots
pdf_data <- data.frame(x = x_vals, Theoretical_PDF = dgamma(x_vals,
shape_gamma, scale = scale_gamma))
empirical_pdf1 <- density(gamma_samples1)
empirical_pdf2 <- density(gamma_samples2)
pdf_data$Empirical_PDF1 <- approxfun(empirical_pdf1)(x_vals)
pdf_data$Empirical_PDF2 <- approxfun(empirical_pdf2)(x_vals)
ggplot(pdf_data, aes(x)) +
geom_line(aes(y = Empirical_PDF1), color = "blue", linetype = "dashed") +
geom_line(aes(y = Empirical_PDF2), color = "green", linetype = "dashed") +
geom_line(aes(y = Theoretical_PDF), color = "red") +
labs(title = "Empirical vs Theoretical PDF", y = "Density", x = "Value") +
theme_minimal()
# Load necessary libraries
library(ggplot2)
library(scales)
library(MASS) # For fitting distributions
# Set the random seed for reproducibility
set.seed(123)
# Step 1: Generate random samples from a chi-square distribution
n <- 1000
df <- 5 # Desired degrees of freedom
chi_square_samples1 <- rchisq(n, df)
chi_square_samples2 <- rchisq(n, df)
# Step 2: Generate random samples from an F distribution
f_samples1 <- rf(df1 = df, df2 = df, n = n)
f_samples2 <- rf(df1 = df, df2 = df, n = n)
# Step 3: Plot histograms of chi-square and F distributions for visual
comparison
hist_data <- data.frame(value = c(chi_square_samples1, chi_square_samples2,
f_samples1, f_samples2),
distribution = rep(c("Chi-Square Sample 1",
"Chi-Square Sample 2", "F Sample 1", "F Sample 2"), each = n))
hist_plot <- ggplot(hist_data, aes(x = value, fill = distribution)) +
geom_histogram(binwidth = 1, alpha = 0.5, color = "black") +
theme_minimal() +
labs(title = "Histogram of Chi-Square and F Distributions", x = "Value", y
= "Frequency") +
scale_fill_manual(values = c("lightblue", "lightgreen", "orange", "red")) +
theme(legend.position = "top", legend.title = element_blank())
print(hist_plot)
# Step 4: Compute mean and variance of both distributions
chi_square_mean1 <- mean(chi_square_samples1)
chi_square_variance1 <- var(chi_square_samples1)
chi_square_mean2 <- mean(chi_square_samples2)
chi_square_variance2 <- var(chi_square_samples2)
f_mean <- df / (df - 2)
f_variance <- (2 * (df^2) * (2 * df)) / ((df - 2)^2 * (df - 4))
cat("Chi-Square Sample 1:\n")
cat(" Mean:", chi_square_mean1, "\n")
cat(" Variance:", chi_square_variance1, "\n\n")
cat("Chi-Square Sample 2:\n")
cat(" Mean:", chi_square_mean2, "\n")
cat(" Variance:", chi_square_variance2, "\n\n")
cat("Fitted F distribution:\n")
cat(" Mean:", f_mean, "\n")
cat(" Variance:", f_variance, "\n")
# Step 5: Create theoretical and empirical CDF plots
x_vals <- seq(0, max(chi_square_samples1, chi_square_samples2, f_samples1,
f_samples2), length.out = 100)
empirical_cdf1_chi <- ecdf(chi_square_samples1)(x_vals)
empirical_cdf2_chi <- ecdf(chi_square_samples2)(x_vals)
theoretical_cdf_chi <- pchisq(x_vals, df)
empirical_cdf1_f <- ecdf(f_samples1)(x_vals)
empirical_cdf2_f <- ecdf(f_samples2)(x_vals)
theoretical_cdf_f <- pf(x_vals, df1 = df, df2 = df)
cdf_data_chi <- data.frame(x = x_vals, Empirical_CDF_Chi1 =
empirical_cdf1_chi, Empirical_CDF_Chi2 = empirical_cdf2_chi,
Theoretical_CDF_Chi = theoretical_cdf_chi)
cdf_data_f <- data.frame(x = x_vals, Empirical_CDF_F1 = empirical_cdf1_f,
Empirical_CDF_F2 = empirical_cdf2_f,
Theoretical_CDF_F = theoretical_cdf_f)
ggplot(cdf_data_chi, aes(x)) +
geom_step(aes(y = Empirical_CDF_Chi1), color = "blue", linetype = "dashed")
+
geom_step(aes(y = Empirical_CDF_Chi2), color = "green", linetype =
"dashed") +
geom_line(aes(y = Theoretical_CDF_Chi), color = "red") +
labs(title = "Empirical vs Theoretical CDF (Chi-Square)", y =
"Probability", x = "Value") +
theme_minimal()
ggplot(cdf_data_f, aes(x)) +
geom_step(aes(y = Empirical_CDF_F1), color = "blue", linetype = "dashed") +
geom_step(aes(y = Empirical_CDF_F2), color = "green", linetype = "dashed")
+
geom_line(aes(y = Theoretical_CDF_F), color = "orange") +
labs(title = "Empirical vs Theoretical CDF (F)", y = "Probability", x =
"Value") +
theme_minimal()
# Step 6: Create theoretical and empirical PDF plots
pdf_data_chi <- data.frame(x = x_vals, Theoretical_PDF_Chi = dchisq(x_vals,
df))
pdf_data_f <- data.frame(x = x_vals, Theoretical_PDF_F = df * df / (x_vals *
(df + x_vals)^2))
empirical_pdf1_chi <- density(chi_square_samples1)
empirical_pdf2_chi <- density(chi_square_samples2)
empirical_pdf1_f <- density(f_samples1)
empirical_pdf2_f <- density(f_samples2)
pdf_data_chi$Empirical_PDF_Chi1 <- approxfun(empirical_pdf1_chi)(x_vals)
pdf_data_chi$Empirical_PDF_Chi2 <- approxfun(empirical_pdf2_chi)(x_vals)
pdf_data_f$Empirical_PDF_F1 <- approxfun(empirical_pdf1_f)(x_vals)
pdf_data_f$Empirical_PDF_F2 <- approxfun(empirical_pdf2_f)(x_vals)
ggplot(pdf_data_chi, aes(x)) +
geom_line(aes(y = Empirical_PDF_Chi1), color = "blue", linetype = "dashed")
+
geom_line(aes(y = Empirical_PDF_Chi2), color = "green", linetype =
"dashed") +
geom_line(aes(y = Theoretical_PDF_Chi), color = "red") +
labs(title = "Empirical vs Theoretical PDF (Chi-Square)", y = "Density", x
= "Value") +
theme_minimal()
ggplot(pdf_data_f, aes(x)) +
geom_line(aes(y = Empirical_PDF_F1), color = "blue", linetype = "dashed") +
geom_line(aes(y = Empirical_PDF_F2), color = "green", linetype = "dashed")
+
geom_line(aes(y = Theoretical_PDF_F), color = "orange") +
labs(title = "Empirical vs Theoretical PDF (F)", y = "Density", x =
"Value") +
theme_minimal()
