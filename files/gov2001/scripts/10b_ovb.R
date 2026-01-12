# =============================================================================
# Gov 2001: Lecture 10b - Omitted Variable Bias
# Spring 2026
# =============================================================================

# This script demonstrates Omitted Variable Bias (OVB) through simulation.
# Key insight: Short = Long + (Effect of omitted) x (Correlation with included)

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2001)

# -----------------------------------------------------------------------------
# Part 1: Simulate Data with Known Parameters
# -----------------------------------------------------------------------------

cat("=== Simulating the True DGP ===\n\n")

n <- 500

# True parameters
beta_0 <- 20000  # Intercept
beta_1 <- 2000   # Effect of education on wages
beta_2 <- 500    # Effect of ability on wages

# Generate variables
# Ability (unobserved confounder)
ability <- rnorm(n, mean = 100, sd = 15)

# Education (correlated with ability)
# delta_1 = 0.08 (relationship between ability and education)
education <- 12 + 0.08 * ability + rnorm(n, sd = 2)

# Wages (true model)
wages <- beta_0 + beta_1 * education + beta_2 * ability + rnorm(n, sd = 5000)

# Create data frame
data <- data.frame(wages = wages, education = education, ability = ability)

cat("True model:\n")
cat("  Wages = ", beta_0, " + ", beta_1, "*Education + ", beta_2, "*Ability + error\n\n", sep = "")

# -----------------------------------------------------------------------------
# Part 2: Short vs Long Regressions
# -----------------------------------------------------------------------------

cat("=== Short vs Long Regressions ===\n\n")

# Short regression (omitting ability)
short <- lm(wages ~ education, data = data)

# Long regression (including ability)
long <- lm(wages ~ education + ability, data = data)

cat("Short regression (omitting ability):\n")
cat("  Intercept: ", round(coef(short)[1], 2), "\n")
cat("  Education: ", round(coef(short)["education"], 2), "\n\n")

cat("Long regression (including ability):\n")
cat("  Intercept: ", round(coef(long)[1], 2), "\n")
cat("  Education: ", round(coef(long)["education"], 2), "\n")
cat("  Ability:   ", round(coef(long)["ability"], 2), "\n\n")

cat("True education effect: ", beta_1, "\n")
cat("Short gives:", round(coef(short)["education"], 2), "(biased!)\n")
cat("Long gives: ", round(coef(long)["education"], 2), "(close to true)\n")

# -----------------------------------------------------------------------------
# Part 3: The OVB Formula
# -----------------------------------------------------------------------------

cat("\n=== The OVB Formula ===\n\n")

# Auxiliary regression: ability on education
aux <- lm(ability ~ education, data = data)

# The OVB formula: Short = Long + beta_2 * delta_1
delta_1 <- coef(aux)["education"]
beta_2_hat <- coef(long)["ability"]
predicted_bias <- beta_2_hat * delta_1

cat("Auxiliary regression (ability ~ education):\n")
cat("  delta_1 = ", round(delta_1, 4), "\n\n")

cat("OVB Formula Verification:\n")
cat("  Short coefficient:     ", round(coef(short)["education"], 2), "\n")
cat("  Long coefficient:      ", round(coef(long)["education"], 2), "\n")
cat("  beta_2 (ability coef): ", round(beta_2_hat, 2), "\n")
cat("  delta_1 (aux slope):   ", round(delta_1, 4), "\n")
cat("  Predicted bias:        ", round(predicted_bias, 2), "\n")
cat("  Long + bias:           ", round(coef(long)["education"] + predicted_bias, 2), "\n\n")

cat("Check: Short = Long + beta_2 * delta_1\n")
cat("       ", round(coef(short)["education"], 2), " = ",
    round(coef(long)["education"], 2), " + ",
    round(predicted_bias, 2), " = ",
    round(coef(long)["education"] + predicted_bias, 2), "\n")

# -----------------------------------------------------------------------------
# Part 4: Direction of Bias
# -----------------------------------------------------------------------------

cat("\n=== Direction of Bias ===\n\n")

cat("Bias = beta_2 * delta_1\n\n")

cat("In our example:\n")
cat("  beta_2 > 0: Higher ability -> higher wages (positive)\n")
cat("  delta_1 > 0: Higher ability -> more education (positive)\n")
cat("  Bias = (+) x (+) = POSITIVE\n\n")

cat("Result: Short regression OVERSTATES the effect of education.\n")
cat("  True effect: ", beta_1, "\n")
cat("  Biased estimate: ", round(coef(short)["education"], 2), "\n")
cat("  Bias magnitude: ", round(coef(short)["education"] - beta_1, 2), "\n")

# -----------------------------------------------------------------------------
# Part 5: When There's No Bias
# -----------------------------------------------------------------------------

cat("\n=== When There's No Bias ===\n\n")

# Case 1: Omitted variable doesn't affect Y (beta_2 = 0)
cat("Case 1: If ability doesn't affect wages (beta_2 = 0)\n")

# Simulate
wages_no_effect <- beta_0 + beta_1 * education + rnorm(n, sd = 5000)
short_no_effect <- lm(wages_no_effect ~ education)

cat("  Short coefficient: ", round(coef(short_no_effect)["education"], 2), "\n")
cat("  True effect: ", beta_1, "\n")
cat("  No bias even though ability is correlated with education!\n\n")

# Case 2: Omitted variable uncorrelated with X (delta_1 = 0)
cat("Case 2: If ability is uncorrelated with education\n")

# Simulate independent ability
ability_indep <- rnorm(n, mean = 100, sd = 15)
wages_indep <- beta_0 + beta_1 * education + beta_2 * ability_indep + rnorm(n, sd = 5000)
short_indep <- lm(wages_indep ~ education)

cat("  Short coefficient: ", round(coef(short_indep)["education"], 2), "\n")
cat("  True effect: ", beta_1, "\n")
cat("  No bias even though ability affects wages!\n")

# -----------------------------------------------------------------------------
# Part 6: Visualizations
# -----------------------------------------------------------------------------

cat("\n=== Creating Visualizations ===\n")

par(mfrow = c(2, 2))

# Plot 1: Short regression
plot(education, wages, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Short Regression (Biased)",
     xlab = "Education", ylab = "Wages")
abline(short, col = "red", lwd = 2)
legend("topleft",
       legend = paste0("Slope = ", round(coef(short)["education"], 0)),
       col = "red", lwd = 2, bty = "n")

# Plot 2: Long regression
plot(education, wages, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Long Regression (Unbiased)",
     xlab = "Education", ylab = "Wages")
# Note: Can't easily plot multivariate regression line
abline(a = coef(long)[1] + coef(long)["ability"] * mean(ability),
       b = coef(long)["education"], col = "forestgreen", lwd = 2)
legend("topleft",
       legend = paste0("Slope = ", round(coef(long)["education"], 0)),
       col = "forestgreen", lwd = 2, bty = "n")

# Plot 3: Ability vs Education (the confounding)
plot(education, ability, pch = 16, col = rgb(0, 0, 1, 0.3),
     main = "Confounding: Ability ~ Education",
     xlab = "Education", ylab = "Ability")
abline(aux, col = "purple", lwd = 2)
legend("topleft",
       legend = paste0("delta_1 = ", round(delta_1, 3)),
       col = "purple", lwd = 2, bty = "n")

# Plot 4: DAG representation (conceptual)
plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))
title("Omitted Variable Bias DAG")

# Draw boxes
rect(1, 4, 3, 6)
text(2, 5, "Education\n(X)")

rect(7, 4, 9, 6)
text(8, 5, "Wages\n(Y)")

rect(4, 7, 6, 9)
text(5, 8, "Ability\n(Omitted)")

# Draw arrows
arrows(3, 5, 7, 5, lwd = 2, col = "red")
text(5, 4.5, expression(beta[1]), col = "red")

arrows(5, 7, 2.5, 6, lwd = 2, col = "blue")
text(3.2, 7, expression(delta[1]), col = "blue")

arrows(5, 7, 7.5, 6, lwd = 2, col = "blue")
text(6.8, 7, expression(beta[2]), col = "blue")

par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# Part 7: Sign Table for Bias Direction
# -----------------------------------------------------------------------------

cat("\n=== Sign Table for Bias Direction ===\n\n")

cat("Bias = beta_2 x delta_1\n\n")
cat("                 | delta_1 > 0        | delta_1 < 0\n")
cat("                 | (X, Omitted +corr) | (X, Omitted -corr)\n")
cat("-----------------+--------------------+-------------------\n")
cat("beta_2 > 0       | Positive bias      | Negative bias\n")
cat("(Omitted -> Y +) | Short > True       | Short < True\n")
cat("-----------------+--------------------+-------------------\n")
cat("beta_2 < 0       | Negative bias      | Positive bias\n")
cat("(Omitted -> Y -) | Short < True       | Short > True\n")

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n\n=== KEY TAKEAWAYS ===\n")
cat("1. OVB Formula: Short = Long + beta_2 * delta_1\n")
cat("2. For bias, BOTH conditions needed:\n")
cat("   - Omitted variable affects Y (beta_2 != 0)\n")
cat("   - Omitted variable correlates with X (delta_1 != 0)\n")
cat("3. Direction: sign(bias) = sign(beta_2) * sign(delta_1)\n")
cat("4. Classic example: Ability biases returns to education upward\n")
