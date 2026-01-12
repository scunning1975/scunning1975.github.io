# =============================================================================
# Gov 2001: Lecture 05b - Central Limit Theorem
# Spring 2026
# =============================================================================

# This script demonstrates the Central Limit Theorem through simulation.
# Key insight: Sample means are approximately normal, regardless of the
# population distribution, when n is large enough.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2026)  # For reproducibility

# -----------------------------------------------------------------------------
# CLT Simulation: Different Population Distributions
# -----------------------------------------------------------------------------

# Function to simulate sampling distribution of the mean
simulate_sampling_dist <- function(pop_sample_fn, n_obs, n_samples = 10000) {
  sample_means <- replicate(n_samples, mean(pop_sample_fn(n_obs)))
  return(sample_means)
}

# We'll try different population distributions:
# 1. Uniform (symmetric, not normal)
# 2. Exponential (right-skewed)
# 3. Bernoulli (discrete, 0/1)

# -----------------------------------------------------------------------------
# Example 1: Uniform Distribution
# -----------------------------------------------------------------------------

# Population: Uniform(0, 1)
# True mean: 0.5
# True variance: 1/12

cat("=== Uniform(0,1) Population ===\n")
cat("True mean: 0.5\n")
cat("True variance: 0.0833\n\n")

# Simulate for different sample sizes
for (n in c(5, 30, 100)) {
  means <- simulate_sampling_dist(function(n) runif(n, 0, 1), n)
  cat(sprintf("n = %3d: Mean of means = %.4f, SD of means = %.4f\n",
              n, mean(means), sd(means)))
  cat(sprintf("         Theory predicts SD = %.4f\n\n", sqrt((1/12)/n)))
}

# -----------------------------------------------------------------------------
# Example 2: Exponential Distribution (Skewed!)
# -----------------------------------------------------------------------------

# Population: Exponential(rate = 1)
# True mean: 1
# True variance: 1

cat("=== Exponential(1) Population (Right-Skewed) ===\n")
cat("True mean: 1\n")
cat("True variance: 1\n\n")

for (n in c(5, 30, 100)) {
  means <- simulate_sampling_dist(function(n) rexp(n, rate = 1), n)
  cat(sprintf("n = %3d: Mean of means = %.4f, SD of means = %.4f\n",
              n, mean(means), sd(means)))
  cat(sprintf("         Theory predicts SD = %.4f\n\n", sqrt(1/n)))
}

# -----------------------------------------------------------------------------
# Example 3: Bernoulli Distribution (Discrete!)
# -----------------------------------------------------------------------------

# Population: Bernoulli(0.3)
# True mean: 0.3
# True variance: 0.3 * 0.7 = 0.21

cat("=== Bernoulli(0.3) Population (Discrete) ===\n")
cat("True mean: 0.3\n")
cat("True variance: 0.21\n\n")

for (n in c(5, 30, 100)) {
  means <- simulate_sampling_dist(function(n) rbinom(n, 1, 0.3), n)
  cat(sprintf("n = %3d: Mean of means = %.4f, SD of means = %.4f\n",
              n, mean(means), sd(means)))
  cat(sprintf("         Theory predicts SD = %.4f\n\n", sqrt(0.21/n)))
}

# -----------------------------------------------------------------------------
# Visualizing the CLT
# -----------------------------------------------------------------------------

# Set up 3x3 plot: rows = distribution, cols = sample size
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))

sample_sizes <- c(5, 30, 100)
n_sims <- 10000

# Row 1: Uniform
for (n in sample_sizes) {
  means <- simulate_sampling_dist(function(n) runif(n, 0, 1), n, n_sims)
  hist(means, breaks = 40, probability = TRUE, col = "lightblue",
       main = paste0("Uniform: n = ", n), xlab = "Sample Mean")
  # Overlay theoretical normal
  x <- seq(min(means), max(means), length.out = 100)
  lines(x, dnorm(x, mean = 0.5, sd = sqrt((1/12)/n)), col = "red", lwd = 2)
}

# Row 2: Exponential
for (n in sample_sizes) {
  means <- simulate_sampling_dist(function(n) rexp(n, 1), n, n_sims)
  hist(means, breaks = 40, probability = TRUE, col = "lightgreen",
       main = paste0("Exponential: n = ", n), xlab = "Sample Mean")
  x <- seq(min(means), max(means), length.out = 100)
  lines(x, dnorm(x, mean = 1, sd = sqrt(1/n)), col = "red", lwd = 2)
}

# Row 3: Bernoulli
for (n in sample_sizes) {
  means <- simulate_sampling_dist(function(n) rbinom(n, 1, 0.3), n, n_sims)
  hist(means, breaks = 40, probability = TRUE, col = "lightyellow",
       main = paste0("Bernoulli: n = ", n), xlab = "Sample Mean")
  x <- seq(min(means), max(means), length.out = 100)
  lines(x, dnorm(x, mean = 0.3, sd = sqrt(0.21/n)), col = "red", lwd = 2)
}

par(mfrow = c(1, 1))  # Reset

# -----------------------------------------------------------------------------
# The CLT in Action: Confidence Interval Coverage
# -----------------------------------------------------------------------------

# If CLT works, 95% CIs should cover the true mean 95% of the time

cat("\n=== 95% CI Coverage Simulation ===\n")

true_mean <- 1  # Exponential(1)
n <- 50
n_sims <- 10000

# Function to check if CI covers true mean
check_coverage <- function() {
  sample <- rexp(n, rate = 1)
  x_bar <- mean(sample)
  se <- sd(sample) / sqrt(n)
  ci_lower <- x_bar - 1.96 * se
  ci_upper <- x_bar + 1.96 * se
  return(ci_lower <= true_mean & true_mean <= ci_upper)
}

coverage <- mean(replicate(n_sims, check_coverage()))
cat(sprintf("With n = %d from Exponential(1):\n", n))
cat(sprintf("  95%% CI coverage rate: %.1f%%\n", coverage * 100))
cat("  (Should be close to 95%)\n")

# -----------------------------------------------------------------------------
# Key Takeaway
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAY ===\n")
cat("The CLT tells us that sample means are approximately normal:\n")
cat("  X_bar ~ N(mu, sigma^2/n)\n")
cat("This works regardless of the population distribution!\n")
cat("The approximation gets better as n increases.\n")
