# =============================================================================
# Gov 2001: Lecture 08a - The Best Linear Predictor
# Spring 2026
# =============================================================================

# This script demonstrates that the BLP formula gives the same answer as OLS.
# Key insight: OLS is just the sample analog of the population BLP.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

# Install wooldridge package if you don't have it
# install.packages("wooldridge")

library(wooldridge)
data(wage1)

# Look at the data
head(wage1[, c("wage", "educ", "exper")])
# wage = average hourly earnings
# educ = years of education
# exper = years of work experience

# -----------------------------------------------------------------------------
# Part 1: Calculate BLP coefficients "by hand"
# -----------------------------------------------------------------------------

# The BLP slope formula: beta = Cov(X, Y) / Var(X)
beta_blp <- cov(wage1$educ, wage1$wage) / var(wage1$educ)

# The BLP intercept: alpha = E[Y] - beta * E[X]
alpha_blp <- mean(wage1$wage) - beta_blp * mean(wage1$educ)

cat("BLP coefficients (by hand):\n")
cat("  Intercept:", round(alpha_blp, 3), "\n")
cat("  Slope:    ", round(beta_blp, 3), "\n\n")

# -----------------------------------------------------------------------------
# Part 2: Compare to lm()
# -----------------------------------------------------------------------------

ols <- lm(wage ~ educ, data = wage1)

cat("OLS coefficients (from lm):\n")
print(coef(ols))

# They match!
cat("\nDifference (should be ~0):\n")
cat("  Intercept:", alpha_blp - coef(ols)[1], "\n")
cat("  Slope:    ", beta_blp - coef(ols)[2], "\n")

# -----------------------------------------------------------------------------
# Part 3: Interpretation
# -----------------------------------------------------------------------------

cat("\n--- Interpretation ---\n")
cat("Each additional year of education is associated with\n")
cat("$", round(beta_blp, 2), " higher hourly wages.\n\n", sep = "")

cat("Someone with 12 years of education has predicted wage:\n")
cat("  ", round(alpha_blp, 2), " + ", round(beta_blp, 2), " * 12 = $",
    round(alpha_blp + beta_blp * 12, 2), "\n\n", sep = "")

cat("Someone with 16 years of education has predicted wage:\n")
cat("  ", round(alpha_blp, 2), " + ", round(beta_blp, 2), " * 16 = $",
    round(alpha_blp + beta_blp * 16, 2), "\n", sep = "")

# -----------------------------------------------------------------------------
# Part 4: Visualize
# -----------------------------------------------------------------------------

plot(wage1$educ, wage1$wage,
     xlab = "Years of Education",
     ylab = "Hourly Wage ($)",
     main = "BLP: Wage vs Education",
     pch = 16, col = rgb(0, 0, 0, 0.3))

# Add the BLP line
abline(a = alpha_blp, b = beta_blp, col = "forestgreen", lwd = 2)

# Add legend
legend("topleft",
       legend = paste0("BLP: wage = ", round(alpha_blp, 2), " + ",
                       round(beta_blp, 2), " * educ"),
       col = "forestgreen", lwd = 2)

# -----------------------------------------------------------------------------
# Part 5: Verify residual properties
# -----------------------------------------------------------------------------

# Calculate residuals
resid_blp <- wage1$wage - alpha_blp - beta_blp * wage1$educ

cat("\n--- Residual Properties ---\n")
cat("E[u] =", round(mean(resid_blp), 10), "(should be ~0)\n")
cat("Cov(u, X) =", round(cov(resid_blp, wage1$educ), 10), "(should be ~0)\n")

# These are the defining properties of BLP residuals!
