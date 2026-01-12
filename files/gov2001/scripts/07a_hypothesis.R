# =============================================================================
# Gov 2001: Lecture 07a - Hypothesis Testing
# Spring 2026
# =============================================================================

# This script demonstrates hypothesis testing concepts through simulation.
# Key concepts: null hypothesis, test statistic, p-value, significance level

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2026)

# -----------------------------------------------------------------------------
# Example: Testing a Population Mean
# -----------------------------------------------------------------------------

# Suppose we want to test H0: mu = 50 vs H1: mu != 50

# Generate sample data (true mu = 52, so H0 is false)
n <- 30
true_mu <- 52
true_sigma <- 10
sample_data <- rnorm(n, mean = true_mu, sd = true_sigma)

cat("=== Hypothesis Test: Is mu = 50? ===\n")
cat(sprintf("Sample mean: %.2f\n", mean(sample_data)))
cat(sprintf("Sample SD: %.2f\n", sd(sample_data)))
cat(sprintf("Sample size: %d\n\n", n))

# -----------------------------------------------------------------------------
# Manual Calculation of Test Statistic
# -----------------------------------------------------------------------------

# Under H0: mu = 50
mu_0 <- 50

# Test statistic: t = (x_bar - mu_0) / (s / sqrt(n))
x_bar <- mean(sample_data)
s <- sd(sample_data)
se <- s / sqrt(n)
t_stat <- (x_bar - mu_0) / se

cat("=== Manual Calculation ===\n")
cat(sprintf("Null hypothesis: mu = %.0f\n", mu_0))
cat(sprintf("Standard error: %.3f\n", se))
cat(sprintf("Test statistic: t = %.3f\n", t_stat))

# P-value (two-sided test)
p_value <- 2 * pt(-abs(t_stat), df = n - 1)
cat(sprintf("P-value (two-sided): %.4f\n", p_value))

# Decision at alpha = 0.05
alpha <- 0.05
if (p_value < alpha) {
  cat(sprintf("Decision: Reject H0 at alpha = %.2f\n", alpha))
} else {
  cat(sprintf("Decision: Fail to reject H0 at alpha = %.2f\n", alpha))
}

# -----------------------------------------------------------------------------
# Using t.test() in R
# -----------------------------------------------------------------------------

cat("\n=== Using t.test() ===\n")
result <- t.test(sample_data, mu = 50)
print(result)

# -----------------------------------------------------------------------------
# Understanding the P-value Through Simulation
# -----------------------------------------------------------------------------

cat("\n=== Understanding P-values ===\n")
cat("The p-value answers: If H0 were true, how often would we see\n")
cat("a test statistic as extreme as what we observed?\n\n")

# Simulate: If mu really equals 50, what's the distribution of t-statistics?
n_sims <- 10000
t_stats_null <- numeric(n_sims)

for (i in 1:n_sims) {
  # Generate data under H0 (mu = 50)
  null_sample <- rnorm(n, mean = 50, sd = 10)
  t_stats_null[i] <- (mean(null_sample) - 50) / (sd(null_sample) / sqrt(n))
}

# What proportion of null t-stats are as extreme as ours?
simulated_p <- mean(abs(t_stats_null) >= abs(t_stat))
cat(sprintf("Our observed t-statistic: %.3f\n", t_stat))
cat(sprintf("Simulated p-value: %.4f\n", simulated_p))
cat(sprintf("Exact p-value: %.4f\n", p_value))

# Visualize
hist(t_stats_null, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Distribution of t under H0", xlab = "t-statistic")
abline(v = c(-abs(t_stat), abs(t_stat)), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Observed |t|", col = "red", lty = 2, lwd = 2)

# -----------------------------------------------------------------------------
# Type I and Type II Errors
# -----------------------------------------------------------------------------

cat("\n=== Type I Error Rate (Size) ===\n")
cat("If H0 is true, we should reject only alpha proportion of the time.\n\n")

# Simulate many tests when H0 IS true
n_sims <- 10000
rejections <- 0

for (i in 1:n_sims) {
  # Generate data where H0 is true (mu = 50)
  sample <- rnorm(30, mean = 50, sd = 10)
  p <- t.test(sample, mu = 50)$p.value
  if (p < 0.05) rejections <- rejections + 1
}

type1_rate <- rejections / n_sims
cat(sprintf("Rejection rate when H0 is true: %.3f\n", type1_rate))
cat("(Should be close to 0.05)\n")

# -----------------------------------------------------------------------------
# Power: Probability of Rejecting When H0 is False
# -----------------------------------------------------------------------------

cat("\n=== Power (Rejecting when H0 is FALSE) ===\n")

# True mu = 52, but we test H0: mu = 50
true_mus <- c(50, 51, 52, 53, 55)
n_sims <- 5000

cat("True mu  |  Power (rejection rate)\n")
cat("---------|-----------------------\n")

for (true_mu in true_mus) {
  rejections <- 0
  for (i in 1:n_sims) {
    sample <- rnorm(30, mean = true_mu, sd = 10)
    p <- t.test(sample, mu = 50)$p.value
    if (p < 0.05) rejections <- rejections + 1
  }
  power <- rejections / n_sims
  cat(sprintf("   %2d    |    %.3f\n", true_mu, power))
}

cat("\nNote: When true_mu = 50 (H0 true), 'power' is actually Type I error.\n")
cat("Power increases as true_mu moves away from 50.\n")

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAYS ===\n")
cat("1. P-value = probability of seeing data this extreme IF H0 is true\n")
cat("2. Small p-value -> evidence against H0\n")
cat("3. alpha = maximum acceptable Type I error rate\n")
cat("4. Power = probability of rejecting H0 when it's false\n")
cat("5. Failing to reject H0 is NOT the same as proving H0 true\n")
