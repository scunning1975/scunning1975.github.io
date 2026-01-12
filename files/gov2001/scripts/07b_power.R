# =============================================================================
# Gov 2001: Lecture 07b - Statistical Power
# Spring 2026
# =============================================================================

# This script demonstrates statistical power and sample size calculations.
# Key insight: Power depends on effect size, sample size, and alpha.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2026)

# -----------------------------------------------------------------------------
# What is Power?
# -----------------------------------------------------------------------------

cat("=== What is Power? ===\n")
cat("Power = P(reject H0 | H0 is false)\n")
cat("     = P(detect an effect when there IS an effect)\n")
cat("     = 1 - P(Type II error)\n\n")

# -----------------------------------------------------------------------------
# Simulating Power
# -----------------------------------------------------------------------------

# Function to estimate power via simulation
estimate_power <- function(true_mu, null_mu = 0, sigma = 1, n = 30,
                           alpha = 0.05, n_sims = 5000) {
  rejections <- 0
  for (i in 1:n_sims) {
    sample <- rnorm(n, mean = true_mu, sd = sigma)
    p_value <- t.test(sample, mu = null_mu)$p.value
    if (p_value < alpha) rejections <- rejections + 1
  }
  return(rejections / n_sims)
}

# -----------------------------------------------------------------------------
# Power vs Effect Size
# -----------------------------------------------------------------------------

cat("=== Power vs Effect Size ===\n")
cat("Testing H0: mu = 0, with sigma = 1, n = 30, alpha = 0.05\n\n")

effect_sizes <- seq(0, 1, by = 0.1)
powers <- sapply(effect_sizes, function(d) estimate_power(true_mu = d, n = 30))

cat("Effect Size (d) | Power\n")
cat("----------------|-------\n")
for (i in seq_along(effect_sizes)) {
  cat(sprintf("      %.1f        | %.3f\n", effect_sizes[i], powers[i]))
}

# Plot
plot(effect_sizes, powers, type = "b", pch = 19, col = "steelblue",
     xlab = "Effect Size (true mu - null mu)", ylab = "Power",
     main = "Power vs Effect Size (n = 30)")
abline(h = 0.80, col = "red", lty = 2)
legend("bottomright", legend = "80% power threshold", col = "red", lty = 2)

# -----------------------------------------------------------------------------
# Power vs Sample Size
# -----------------------------------------------------------------------------

cat("\n=== Power vs Sample Size ===\n")
cat("Testing H0: mu = 0, with true mu = 0.5, sigma = 1, alpha = 0.05\n\n")

sample_sizes <- c(10, 20, 30, 50, 100, 200)
powers_n <- sapply(sample_sizes, function(n) estimate_power(true_mu = 0.5, n = n))

cat("Sample Size | Power\n")
cat("------------|-------\n")
for (i in seq_along(sample_sizes)) {
  cat(sprintf("    %3d     | %.3f\n", sample_sizes[i], powers_n[i]))
}

# Plot
plot(sample_sizes, powers_n, type = "b", pch = 19, col = "forestgreen",
     xlab = "Sample Size", ylab = "Power",
     main = "Power vs Sample Size (effect = 0.5)")
abline(h = 0.80, col = "red", lty = 2)

# -----------------------------------------------------------------------------
# Power vs Alpha
# -----------------------------------------------------------------------------

cat("\n=== Power vs Alpha ===\n")
cat("Testing H0: mu = 0, with true mu = 0.5, sigma = 1, n = 30\n\n")

alphas <- c(0.01, 0.05, 0.10, 0.20)
powers_alpha <- sapply(alphas, function(a)
  estimate_power(true_mu = 0.5, n = 30, alpha = a))

cat("Alpha | Power\n")
cat("------|-------\n")
for (i in seq_along(alphas)) {
  cat(sprintf(" %.2f  | %.3f\n", alphas[i], powers_alpha[i]))
}

cat("\nNote: Higher alpha -> higher power, but also higher Type I error!\n")

# -----------------------------------------------------------------------------
# Power Analysis: How Many Subjects Do I Need?
# -----------------------------------------------------------------------------

cat("\n=== Sample Size Calculation ===\n")
cat("Goal: Find n needed for 80% power\n\n")

# Analytical formula for z-test (approximate):
# n = ((z_alpha + z_beta) * sigma / delta)^2
# where delta = true_mu - null_mu

# For 80% power (beta = 0.20), alpha = 0.05 (two-sided)
z_alpha <- qnorm(1 - 0.05/2)  # 1.96
z_beta <- qnorm(0.80)          # 0.84
sigma <- 1

cat("Using analytical formula (z-test approximation):\n")
cat(sprintf("z_alpha/2 = %.2f, z_beta = %.2f\n\n", z_alpha, z_beta))

effect_sizes <- c(0.2, 0.3, 0.5, 0.8, 1.0)
cat("Effect Size | Required n\n")
cat("------------|------------\n")
for (d in effect_sizes) {
  n_required <- ((z_alpha + z_beta) * sigma / d)^2
  cat(sprintf("    %.1f      |    %3.0f\n", d, ceiling(n_required)))
}

# -----------------------------------------------------------------------------
# Using power.t.test() in R
# -----------------------------------------------------------------------------

cat("\n=== Using power.t.test() ===\n")

# Find required sample size for 80% power
result <- power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.80,
                       type = "one.sample")
cat("\nFor effect size = 0.5, 80% power:\n")
print(result)

# Find power for a given sample size
result2 <- power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05,
                        type = "one.sample")
cat("\nFor n = 30, effect size = 0.5:\n")
print(result2)

# -----------------------------------------------------------------------------
# Power Curves
# -----------------------------------------------------------------------------

cat("\n=== Power Curves ===\n")

# Create power curves for different sample sizes
par(mar = c(4, 4, 3, 1))
effect_seq <- seq(0, 1, length.out = 50)

plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Effect Size", ylab = "Power",
     main = "Power Curves for Different Sample Sizes")

colors <- c("red", "orange", "forestgreen", "blue", "purple")
ns <- c(10, 20, 50, 100, 200)

for (i in seq_along(ns)) {
  powers <- sapply(effect_seq, function(d) {
    if (d == 0) return(0.05)  # Type I error at d = 0
    power.t.test(n = ns[i], delta = d, sd = 1, sig.level = 0.05,
                 type = "one.sample")$power
  })
  lines(effect_seq, powers, col = colors[i], lwd = 2)
}

abline(h = 0.80, col = "gray", lty = 2)
legend("bottomright", legend = paste("n =", ns), col = colors, lwd = 2)

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAYS ===\n")
cat("1. Power increases with effect size\n")
cat("2. Power increases with sample size\n")
cat("3. Power increases with alpha (but at cost of more Type I errors)\n")
cat("4. Convention: aim for 80% power\n")
cat("5. Use power analysis BEFORE collecting data to determine sample size\n")
cat("6. Underpowered studies waste resources and may miss real effects\n")
