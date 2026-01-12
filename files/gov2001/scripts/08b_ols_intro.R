# =============================================================================
# Gov 2001: Lecture 08b - OLS as Sample BLP
# Spring 2026
# =============================================================================

# This script demonstrates that OLS is the sample analog of the BLP.
# Key insight: The plug-in principle replaces population quantities with
# sample analogs.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2026)

# We'll use the wooldridge package for real data
# install.packages("wooldridge")
library(wooldridge)
data(wage1)

# -----------------------------------------------------------------------------
# The Plug-In Principle
# -----------------------------------------------------------------------------

cat("=== The Plug-In Principle ===\n\n")

# Population quantities we don't know:
# E[Y], E[X], Var(X), Cov(X, Y)

# Sample analogs we can compute:
x_bar <- mean(wage1$educ)
y_bar <- mean(wage1$wage)
var_x <- var(wage1$educ)
cov_xy <- cov(wage1$educ, wage1$wage)

cat("Sample analogs:\n")
cat("  X_bar (mean education):", round(x_bar, 3), "\n")
cat("  Y_bar (mean wage):     ", round(y_bar, 3), "\n")
cat("  Var(X):                ", round(var_x, 3), "\n")
cat("  Cov(X, Y):             ", round(cov_xy, 3), "\n\n")

# -----------------------------------------------------------------------------
# Computing OLS by Hand
# -----------------------------------------------------------------------------

cat("=== Computing OLS by Hand ===\n\n")

# OLS slope: beta_hat = Cov(X, Y) / Var(X)
beta_hat <- cov_xy / var_x

# OLS intercept: alpha_hat = Y_bar - beta_hat * X_bar
alpha_hat <- y_bar - beta_hat * x_bar

cat("OLS coefficients (by hand):\n")
cat("  Intercept:", round(alpha_hat, 4), "\n")
cat("  Slope:    ", round(beta_hat, 4), "\n\n")

# -----------------------------------------------------------------------------
# Verify Against lm()
# -----------------------------------------------------------------------------

cat("=== Verify Against lm() ===\n\n")

ols <- lm(wage ~ educ, data = wage1)

cat("OLS coefficients (from lm):\n")
print(coef(ols))

cat("\nDifference (should be ~0):\n")
cat("  Intercept:", alpha_hat - coef(ols)[1], "\n")
cat("  Slope:    ", beta_hat - coef(ols)[2], "\n")

# -----------------------------------------------------------------------------
# Why "Least Squares"?
# -----------------------------------------------------------------------------

cat("\n=== Why 'Least Squares'? ===\n\n")

# OLS minimizes the sum of squared residuals
# Let's verify this numerically

# Define sum of squared residuals function
ssr <- function(params, x, y) {
  a <- params[1]
  b <- params[2]
  sum((y - a - b * x)^2)
}

# Minimize numerically
opt <- optim(c(0, 0), ssr, x = wage1$educ, y = wage1$wage)

cat("Coefficients from numerical minimization:\n")
cat("  Intercept:", round(opt$par[1], 4), "\n")
cat("  Slope:    ", round(opt$par[2], 4), "\n\n")

cat("Coefficients from OLS formula:\n")
cat("  Intercept:", round(alpha_hat, 4), "\n")
cat("  Slope:    ", round(beta_hat, 4), "\n")

cat("\nThey match! OLS minimizes the sum of squared residuals.\n")

# -----------------------------------------------------------------------------
# Fitted Values and Residuals
# -----------------------------------------------------------------------------

cat("\n=== Fitted Values and Residuals ===\n\n")

# Fitted values: Y_hat = alpha + beta * X
fitted_vals <- alpha_hat + beta_hat * wage1$educ

# Residuals: e_hat = Y - Y_hat
residuals <- wage1$wage - fitted_vals

cat("First 5 observations:\n")
cat("  Actual wages: ", round(wage1$wage[1:5], 2), "\n")
cat("  Fitted values:", round(fitted_vals[1:5], 2), "\n")
cat("  Residuals:    ", round(residuals[1:5], 2), "\n")

# -----------------------------------------------------------------------------
# Key Properties of Residuals
# -----------------------------------------------------------------------------

cat("\n=== Key Properties of OLS Residuals ===\n\n")

# Property 1: Residuals sum to zero
cat("Sum of residuals:", round(sum(residuals), 10), "(should be ~0)\n")

# Property 2: Residuals are uncorrelated with X
cat("Cov(X, residuals):", round(cov(wage1$educ, residuals), 10), "(should be ~0)\n")

# Alternative: Sum of X times residuals is zero
cat("Sum(X * residuals):", round(sum(wage1$educ * residuals), 10), "(should be ~0)\n")

# -----------------------------------------------------------------------------
# R-squared: Goodness of Fit
# -----------------------------------------------------------------------------

cat("\n=== R-squared: Goodness of Fit ===\n\n")

# Total Sum of Squares
SST <- sum((wage1$wage - mean(wage1$wage))^2)

# Explained Sum of Squares
SSE <- sum((fitted_vals - mean(wage1$wage))^2)

# Residual Sum of Squares
SSR <- sum(residuals^2)

cat("Sum of Squares decomposition:\n")
cat("  SST (Total):   ", round(SST, 2), "\n")
cat("  SSE (Explained):", round(SSE, 2), "\n")
cat("  SSR (Residual): ", round(SSR, 2), "\n")
cat("  SSE + SSR:      ", round(SSE + SSR, 2), "(should equal SST)\n\n")

# R-squared
R2 <- 1 - SSR / SST

cat("R-squared:", round(R2, 4), "\n")
cat("From lm(): ", round(summary(ols)$r.squared, 4), "\n")
cat("Interpretation:", round(R2 * 100, 1), "% of variance in wages\n")
cat("              is 'explained' by education.\n")

# -----------------------------------------------------------------------------
# Multiple Regression Example
# -----------------------------------------------------------------------------

cat("\n=== Multiple Regression ===\n\n")

# Add experience to the model
ols_multi <- lm(wage ~ educ + exper, data = wage1)

cat("Multiple regression coefficients:\n")
print(coef(ols_multi))

cat("\nInterpretation:\n")
cat("  Each additional year of education is associated with\n")
cat("  $", round(coef(ols_multi)["educ"], 2), " higher wages,\n", sep = "")
cat("  holding experience constant.\n")

# -----------------------------------------------------------------------------
# Visualize the OLS Fit
# -----------------------------------------------------------------------------

cat("\n=== Visualization ===\n")

par(mfrow = c(1, 2))

# Plot 1: Scatterplot with regression line
plot(wage1$educ, wage1$wage,
     xlab = "Years of Education",
     ylab = "Hourly Wage ($)",
     main = "OLS: Wage vs Education",
     pch = 16, col = rgb(0, 0, 0, 0.3))
abline(a = alpha_hat, b = beta_hat, col = "forestgreen", lwd = 2)
legend("topleft",
       legend = paste0("Y = ", round(alpha_hat, 2), " + ",
                       round(beta_hat, 2), " * X"),
       col = "forestgreen", lwd = 2, bty = "n")

# Plot 2: Residuals vs fitted values
plot(fitted_vals, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 16, col = rgb(0, 0, 0, 0.3))
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAYS ===\n")
cat("1. OLS is the sample analog of the population BLP\n")
cat("2. OLS minimizes sum of squared residuals\n")
cat("3. Residuals sum to zero and are uncorrelated with X\n")
cat("4. R-squared measures fraction of variance explained\n")
cat("5. The regression line passes through (X_bar, Y_bar)\n")
