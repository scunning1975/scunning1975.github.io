# =============================================================================
# Gov 2001: Lecture 12a - Robust Standard Errors
# Spring 2026
# =============================================================================

# This script demonstrates heteroskedasticity and robust standard errors.
# Key insight: OLS is still unbiased, but classical SEs are wrong.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2001)

# Install if needed:
# install.packages("estimatr")
# install.packages("sandwich")
# install.packages("lmtest")

library(estimatr)
library(sandwich)
library(lmtest)

# -----------------------------------------------------------------------------
# Part 1: Simulating Heteroskedasticity
# -----------------------------------------------------------------------------

cat("=== Simulating Heteroskedasticity ===\n\n")

n <- 500

# X variable
x <- runif(n, 1, 10)

# Heteroskedastic errors: variance increases with X
# sigma_i = 0.5 * x_i (variance is proportional to X)
errors <- rnorm(n, mean = 0, sd = 0.5 * x)

# Y with true beta = 2
y <- 5 + 2 * x + errors

data <- data.frame(y = y, x = x)

cat("True model: Y = 5 + 2*X + error\n")
cat("Error structure: SD(error) = 0.5 * X (heteroskedastic)\n\n")

# Visualize
par(mfrow = c(1, 2))

# Plot 1: Scatterplot showing fanning
plot(x, y, pch = 16, col = rgb(0, 0, 0, 0.4),
     main = "Heteroskedasticity: Variance Increases with X",
     xlab = "X", ylab = "Y")
abline(a = 5, b = 2, col = "red", lwd = 2)

# Plot 2: Residuals vs X
m <- lm(y ~ x, data = data)
plot(x, residuals(m), pch = 16, col = rgb(0, 0, 0, 0.4),
     main = "Residuals vs X (Shows Fanning)",
     xlab = "X", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# Part 2: Classical vs Robust Standard Errors
# -----------------------------------------------------------------------------

cat("\n=== Classical vs Robust Standard Errors ===\n\n")

# Fit OLS with classical SEs
ols_classical <- lm(y ~ x, data = data)

cat("OLS with CLASSICAL standard errors:\n")
print(summary(ols_classical)$coefficients)

# Fit OLS with robust SEs using estimatr
ols_robust <- lm_robust(y ~ x, data = data, se_type = "HC2")

cat("\nOLS with ROBUST standard errors (HC2):\n")
print(summary(ols_robust)$coefficients[, c("Estimate", "Std. Error", "Pr(>|t|)")])

# Compare
cat("\n--- Comparison ---\n")
cat("                 Classical SE   Robust SE\n")
cat("Intercept:       ", round(summary(ols_classical)$coef[1, 2], 4),
    "        ", round(ols_robust$std.error[1], 4), "\n")
cat("x:               ", round(summary(ols_classical)$coef[2, 2], 4),
    "        ", round(ols_robust$std.error[2], 4), "\n")

cat("\nNote: Robust SEs are often LARGER when there's heteroskedasticity\n")
cat("      with high-variance observations having extreme X values.\n")

# -----------------------------------------------------------------------------
# Part 3: Different HC Variants
# -----------------------------------------------------------------------------

cat("\n=== Different HC Variants ===\n\n")

# Compare HC0, HC1, HC2, HC3
se_hc0 <- sqrt(diag(vcovHC(ols_classical, type = "HC0")))["x"]
se_hc1 <- sqrt(diag(vcovHC(ols_classical, type = "HC1")))["x"]
se_hc2 <- sqrt(diag(vcovHC(ols_classical, type = "HC2")))["x"]
se_hc3 <- sqrt(diag(vcovHC(ols_classical, type = "HC3")))["x"]
se_classical <- summary(ols_classical)$coef["x", 2]

cat("Standard errors for coefficient on X:\n\n")
cat("Type        SE        Notes\n")
cat("----------- --------- ----------------------------------\n")
cat("Classical:  ", sprintf("%.4f", se_classical), "   Assumes homoskedasticity\n")
cat("HC0:        ", sprintf("%.4f", se_hc0), "   Original EHW\n")
cat("HC1:        ", sprintf("%.4f", se_hc1), "   Stata default (df correction)\n")
cat("HC2:        ", sprintf("%.4f", se_hc2), "   R default (leverage adjustment)\n")
cat("HC3:        ", sprintf("%.4f", se_hc3), "   Most conservative\n")

# -----------------------------------------------------------------------------
# Part 4: Impact on Inference
# -----------------------------------------------------------------------------

cat("\n=== Impact on Inference ===\n\n")

# t-statistics
t_classical <- coef(ols_classical)["x"] / se_classical
t_robust <- coef(ols_classical)["x"] / se_hc2

cat("Testing H0: beta_x = 0\n\n")
cat("Using classical SE:\n")
cat("  t-statistic: ", round(t_classical, 2), "\n")
cat("  p-value:     ", round(2 * pt(-abs(t_classical), df = n - 2), 4), "\n")

cat("\nUsing robust SE (HC2):\n")
cat("  t-statistic: ", round(t_robust, 2), "\n")
cat("  p-value:     ", round(2 * pt(-abs(t_robust), df = n - 2), 4), "\n")

# Confidence intervals
ci_classical <- confint(ols_classical)["x", ]
ci_robust <- c(coef(ols_classical)["x"] - 1.96 * se_hc2,
               coef(ols_classical)["x"] + 1.96 * se_hc2)

cat("\n95% Confidence Intervals for beta_x:\n")
cat("  Classical: [", round(ci_classical[1], 3), ", ", round(ci_classical[2], 3), "]\n")
cat("  Robust:    [", round(ci_robust[1], 3), ", ", round(ci_robust[2], 3), "]\n")

# -----------------------------------------------------------------------------
# Part 5: Simulation - Coverage Rates
# -----------------------------------------------------------------------------

cat("\n=== Simulation: CI Coverage Rates ===\n\n")

n_sims <- 2000
true_beta <- 2
n <- 100

coverage_classical <- 0
coverage_robust <- 0

for (i in 1:n_sims) {
  # Generate heteroskedastic data
  x <- runif(n, 1, 10)
  errors <- rnorm(n, mean = 0, sd = 0.5 * x)
  y <- 5 + true_beta * x + errors

  # Fit model
  m <- lm(y ~ x)

  # Classical CI
  se_class <- summary(m)$coef["x", 2]
  ci_class <- coef(m)["x"] + c(-1.96, 1.96) * se_class
  if (ci_class[1] <= true_beta & true_beta <= ci_class[2]) {
    coverage_classical <- coverage_classical + 1
  }

  # Robust CI
  se_rob <- sqrt(vcovHC(m, type = "HC2")["x", "x"])
  ci_rob <- coef(m)["x"] + c(-1.96, 1.96) * se_rob
  if (ci_rob[1] <= true_beta & true_beta <= ci_rob[2]) {
    coverage_robust <- coverage_robust + 1
  }
}

cat("95% CI Coverage Rates (", n_sims, " simulations):\n", sep = "")
cat("  Classical SEs:", round(100 * coverage_classical / n_sims, 1), "%\n")
cat("  Robust SEs:   ", round(100 * coverage_robust / n_sims, 1), "%\n")
cat("\nNote: Under heteroskedasticity, classical CIs have WRONG coverage!\n")

# -----------------------------------------------------------------------------
# Part 6: Using coeftest() from lmtest package
# -----------------------------------------------------------------------------

cat("\n=== Using coeftest() for Robust Inference ===\n\n")

m <- lm(y ~ x, data = data)

cat("Classical inference:\n")
print(coeftest(m))

cat("\nRobust inference (HC2):\n")
print(coeftest(m, vcov = vcovHC(m, type = "HC2")))

# -----------------------------------------------------------------------------
# Part 7: Detecting Heteroskedasticity (Visual)
# -----------------------------------------------------------------------------

cat("\n=== Detecting Heteroskedasticity ===\n")

par(mfrow = c(2, 2))

# Homoskedastic data for comparison
x_homo <- runif(n, 1, 10)
y_homo <- 5 + 2 * x_homo + rnorm(n, sd = 2)
m_homo <- lm(y_homo ~ x_homo)

# Plot 1: Residuals vs fitted (homoskedastic)
plot(fitted(m_homo), residuals(m_homo), pch = 16, col = rgb(0, 0.5, 0, 0.4),
     main = "Homoskedastic: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 2: Residuals vs fitted (heteroskedastic)
m_hetero <- lm(y ~ x, data = data)
plot(fitted(m_hetero), residuals(m_hetero), pch = 16, col = rgb(0.5, 0, 0, 0.4),
     main = "Heteroskedastic: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Plot 3: Scale-location (homoskedastic)
plot(fitted(m_homo), sqrt(abs(residuals(m_homo))), pch = 16, col = rgb(0, 0.5, 0, 0.4),
     main = "Homoskedastic: Scale-Location",
     xlab = "Fitted Values", ylab = "sqrt(|Residuals|)")
lines(lowess(fitted(m_homo), sqrt(abs(residuals(m_homo)))), col = "red", lwd = 2)

# Plot 4: Scale-location (heteroskedastic)
plot(fitted(m_hetero), sqrt(abs(residuals(m_hetero))), pch = 16, col = rgb(0.5, 0, 0, 0.4),
     main = "Heteroskedastic: Scale-Location",
     xlab = "Fitted Values", ylab = "sqrt(|Residuals|)")
lines(lowess(fitted(m_hetero), sqrt(abs(residuals(m_hetero)))), col = "red", lwd = 2)

par(mfrow = c(1, 1))

cat("\nLook for 'fanning' pattern in residual plots.\n")
cat("Scale-location plot: horizontal line suggests homoskedasticity.\n")

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAYS ===\n")
cat("1. Heteroskedasticity = error variance varies with X\n")
cat("2. OLS point estimates are still unbiased and consistent\n")
cat("3. Classical standard errors are WRONG under heteroskedasticity\n")
cat("4. Robust (HC) SEs are valid whether or not there's heteroskedasticity\n")
cat("5. Modern advice: Use robust SEs by default\n")
cat("6. In R: use lm_robust() or coeftest() with vcovHC()\n")
