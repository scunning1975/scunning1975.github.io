# =============================================================================
# Gov 2001: Lecture 06b - Confidence Intervals
# Spring 2026
# =============================================================================

# This script demonstrates confidence interval construction and interpretation.
# Key insight: A 95% CI procedure covers the true parameter 95% of the time.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2026)

# -----------------------------------------------------------------------------
# Constructing a Confidence Interval
# -----------------------------------------------------------------------------

# Suppose we have a sample and want to estimate the population mean

# Generate sample data
n <- 50
true_mu <- 10
true_sigma <- 3
sample_data <- rnorm(n, mean = true_mu, sd = true_sigma)

# Point estimate
x_bar <- mean(sample_data)
cat("=== Confidence Interval Construction ===\n")
cat(sprintf("Sample mean: %.3f\n", x_bar))

# Standard error (using sample SD since we don't know population SD)
s <- sd(sample_data)
se <- s / sqrt(n)
cat(sprintf("Sample SD: %.3f\n", s))
cat(sprintf("Standard error: %.3f\n", se))

# 95% CI using t-distribution (correct for unknown variance)
t_crit <- qt(0.975, df = n - 1)
ci_lower <- x_bar - t_crit * se
ci_upper <- x_bar + t_crit * se

cat(sprintf("\n95%% CI (using t): [%.3f, %.3f]\n", ci_lower, ci_upper))
cat(sprintf("True mu = %.3f -- Covered? %s\n", true_mu,
            ifelse(ci_lower <= true_mu & true_mu <= ci_upper, "YES", "NO")))

# Compare to using normal approximation
z_crit <- qnorm(0.975)
ci_lower_z <- x_bar - z_crit * se
ci_upper_z <- x_bar + z_crit * se
cat(sprintf("\n95%% CI (using z): [%.3f, %.3f]\n", ci_lower_z, ci_upper_z))

# -----------------------------------------------------------------------------
# Using t.test() in R
# -----------------------------------------------------------------------------

cat("\n=== Using t.test() ===\n")
result <- t.test(sample_data, conf.level = 0.95)
print(result)

# Extract just the CI
cat(sprintf("\nExtracted CI: [%.3f, %.3f]\n",
            result$conf.int[1], result$conf.int[2]))

# -----------------------------------------------------------------------------
# Simulation: 95% Coverage Rate
# -----------------------------------------------------------------------------

cat("\n=== Simulation: CI Coverage Rate ===\n")

# True population parameters
true_mu <- 50
true_sigma <- 10
n <- 30
n_sims <- 10000

# Function to generate one sample and check if CI covers true_mu
check_coverage <- function() {
  sample <- rnorm(n, mean = true_mu, sd = true_sigma)
  result <- t.test(sample, conf.level = 0.95)
  ci <- result$conf.int
  return(ci[1] <= true_mu & true_mu <= ci[2])
}

# Run simulation
covered <- replicate(n_sims, check_coverage())
coverage_rate <- mean(covered)

cat(sprintf("Simulated %d confidence intervals\n", n_sims))
cat(sprintf("Coverage rate: %.2f%%\n", coverage_rate * 100))
cat("(Should be close to 95%)\n")

# -----------------------------------------------------------------------------
# Visualizing Many Confidence Intervals
# -----------------------------------------------------------------------------

# Generate and visualize 100 CIs
n_vis <- 100
true_mu <- 50
true_sigma <- 10
n <- 30

# Store CIs
ci_data <- data.frame(lower = numeric(n_vis), upper = numeric(n_vis),
                      covers = logical(n_vis))

for (i in 1:n_vis) {
  sample <- rnorm(n, mean = true_mu, sd = true_sigma)
  result <- t.test(sample, conf.level = 0.95)
  ci_data$lower[i] <- result$conf.int[1]
  ci_data$upper[i] <- result$conf.int[2]
  ci_data$covers[i] <- (ci_data$lower[i] <= true_mu & true_mu <= ci_data$upper[i])
}

# Plot
par(mar = c(4, 4, 3, 1))
plot(NULL, xlim = c(40, 60), ylim = c(1, n_vis),
     xlab = "Value", ylab = "Sample", main = "100 Confidence Intervals")
abline(v = true_mu, col = "red", lwd = 2, lty = 2)

for (i in 1:n_vis) {
  color <- ifelse(ci_data$covers[i], "gray50", "red")
  segments(ci_data$lower[i], i, ci_data$upper[i], i, col = color, lwd = 1.5)
}

legend("topright", legend = c("Covers true mu", "Misses true mu"),
       col = c("gray50", "red"), lwd = 2)

cat(sprintf("\nIn visualization: %d/%d CIs cover the true mean\n",
            sum(ci_data$covers), n_vis))

# -----------------------------------------------------------------------------
# Effect of Sample Size on CI Width
# -----------------------------------------------------------------------------

cat("\n=== Effect of Sample Size on CI Width ===\n")

sample_sizes <- c(10, 25, 50, 100, 500)
true_mu <- 50
true_sigma <- 10

for (n in sample_sizes) {
  sample <- rnorm(n, mean = true_mu, sd = true_sigma)
  result <- t.test(sample, conf.level = 0.95)
  width <- diff(result$conf.int)
  cat(sprintf("n = %3d: CI width = %.2f\n", n, width))
}

cat("\nCI width decreases with sqrt(n).\n")
cat("Double n -> width shrinks by factor of sqrt(2) ≈ 1.41\n")

# -----------------------------------------------------------------------------
# Different Confidence Levels
# -----------------------------------------------------------------------------

cat("\n=== Different Confidence Levels ===\n")

sample <- rnorm(50, mean = true_mu, sd = true_sigma)

for (level in c(0.90, 0.95, 0.99)) {
  result <- t.test(sample, conf.level = level)
  width <- diff(result$conf.int)
  cat(sprintf("%d%% CI: [%.2f, %.2f], width = %.2f\n",
              level * 100, result$conf.int[1], result$conf.int[2], width))
}

cat("\nHigher confidence = wider interval.\n")
cat("There's always a tradeoff between confidence and precision.\n")

# -----------------------------------------------------------------------------
# Key Interpretation
# -----------------------------------------------------------------------------

cat("\n=== KEY INTERPRETATION ===\n")
cat("A 95% CI does NOT mean:\n")
cat("  'There's a 95% probability the true mu is in this interval'\n")
cat("\nA 95% CI DOES mean:\n")
cat("  'This procedure produces intervals that contain the true mu 95% of the time'\n")
cat("\nThe true mu is fixed; the interval is random.\n")
