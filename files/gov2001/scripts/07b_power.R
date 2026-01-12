# =============================================================================
# Gov 2001: Lecture 07b - Statistical Power
# Spring 2026
# =============================================================================

# This script demonstrates statistical power and sample size calculations.
# Key insight: Power depends on effect size, sample size, and alpha.
#
# ESTIMATED RUNTIME: ~20-25 seconds on a standard laptop
# (The simulations take time but are important for understanding power)

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
# PRE-STUDY POWER ANALYSIS: A Complete Example
# -----------------------------------------------------------------------------

# This is what you'd do BEFORE collecting data for a study.
# The key insight: You specify the effect size AHEAD OF TIME based on:
#   - Prior literature
#   - Minimum effect that would be practically meaningful
#   - Pilot data

cat("\n=== PRE-STUDY POWER ANALYSIS EXAMPLE ===\n")
cat("Scenario: Planning a GOTV experiment\n\n")

# Step 1: Specify expected effect based on prior research
cat("STEP 1: Determine expected effect size from prior literature\n")
cat("  Review of GOTV experiments:\n")
cat("    - Gerber & Green (2000): 8.7 pp (door-to-door, New Haven)\n")
cat("    - Green, Gerber, Nickerson (2003): 7-10 pp (meta of 6 RCTs)\n")
cat("    - Arceneaux (2005): 2.5 pp (low-salience election)\n")
cat("    - Nickerson (2008): 2.1 pp (Denver, Minneapolis)\n")
cat("    - Green & Gerber (2015) book: 2-5 pp typical range\n\n")
cat("  Decision: Target detecting a 3pp effect (conservative)\n")
cat("  - Control group turnout expected: ~40%\n")
cat("  - Treatment group turnout expected: ~43%\n\n")

expected_effect <- 0.03      # 3 percentage point increase
control_turnout <- 0.40
treatment_turnout <- 0.43

# Step 2: Set target power and significance level
cat("STEP 2: Set target power and alpha\n")
cat("  - Target power: 80% (standard convention)\n")
cat("  - Alpha: 0.05 (two-sided)\n\n")

target_power <- 0.80
alpha <- 0.05

# Step 3: Estimate the standard deviation
# For proportions, SD = sqrt(p*(1-p))
# Pool across groups for rough estimate
pooled_p <- (control_turnout + treatment_turnout) / 2
sigma <- sqrt(pooled_p * (1 - pooled_p))

cat("STEP 3: Estimate variability\n")
cat("  - Using binomial SD: sqrt(p*(1-p))\n")
cat("  - SD estimate:", round(sigma, 3), "\n\n")

# Step 4: Calculate required sample size (per group)
cat("STEP 4: Calculate required sample size\n")

# Using power.t.test as approximation for large samples
power_result <- power.t.test(
  delta = expected_effect,
  sd = sigma,
  sig.level = alpha,
  power = target_power,
  type = "two.sample"
)

n_per_group <- ceiling(power_result$n)

cat("  Required n PER GROUP:", n_per_group, "\n")
cat("  Total sample needed:", 2 * n_per_group, "\n\n")

# Step 5: Simulation-based verification
cat("STEP 5: Verify with simulation\n")

set.seed(2026)
n_sims <- 2000
rejections <- 0

for (i in 1:n_sims) {
  # Simulate the experiment
  control <- rbinom(n_per_group, 1, control_turnout)
  treatment <- rbinom(n_per_group, 1, treatment_turnout)

  # Run the test we'd actually use
  test <- t.test(treatment, control)

  if (test$p.value < alpha) rejections <- rejections + 1
}

simulated_power <- rejections / n_sims

cat("  Simulated power:", round(simulated_power, 3), "\n")
cat("  Target power:   ", target_power, "\n")
cat("  (These should be close)\n\n")

# Step 6: Document your power analysis
cat("STEP 6: Document for pre-registration\n")
cat("--------------------------------------\n")
cat("POWER ANALYSIS SUMMARY\n")
cat("  Hypotheses:\n")
cat("    H0: Treatment effect = 0\n")
cat("    H1: Treatment effect != 0\n")
cat("  Expected effect: 3 percentage points\n")
cat("  Control baseline: 40% turnout\n")
cat("  Alpha: 0.05 (two-sided)\n")
cat("  Target power: 80%\n")
cat("  Required sample: ", 2 * n_per_group, " total\n", sep = "")
cat("                   (", n_per_group, " per group)\n", sep = "")
cat("--------------------------------------\n\n")

# What if the effect is smaller than expected?
cat("SENSITIVITY ANALYSIS: What if effect is smaller?\n")
effects_to_test <- c(0.02, 0.025, 0.03, 0.035, 0.04)

cat("Effect Size | Power with n=", 2 * n_per_group, "\n", sep = "")
cat("------------|---------------\n")

for (eff in effects_to_test) {
  # Simulate with the planned sample size
  power_at_effect <- 0
  for (i in 1:1000) {
    ctrl <- rbinom(n_per_group, 1, control_turnout)
    trt <- rbinom(n_per_group, 1, control_turnout + eff)
    if (t.test(trt, ctrl)$p.value < alpha) power_at_effect <- power_at_effect + 1
  }
  power_at_effect <- power_at_effect / 1000
  cat(sprintf("   %.1f pp    |    %.1f%%\n", eff * 100, power_at_effect * 100))
}

cat("\nThis is why specifying effect size matters:\n")
cat("If the true effect is only 2pp, you'd need a MUCH larger sample.\n")

# -----------------------------------------------------------------------------
# VISUALIZATION: Power Curves for GOTV Experiment
# -----------------------------------------------------------------------------

cat("\n=== Power Curve Visualization ===\n")
cat("Creating power curves for the GOTV example...\n")

# Calculate power analytically for a range of sample sizes and effect sizes
# Using normal approximation for proportions

gotv_power <- function(n_per_group, effect, p0 = 0.40, alpha = 0.05) {
  # Pooled SD for proportions
  p1 <- p0 + effect
  pooled_sd <- sqrt((p0 * (1 - p0) + p1 * (1 - p1)) / 2)
  se <- pooled_sd * sqrt(2 / n_per_group)

  # Critical value and power
  z_crit <- qnorm(1 - alpha / 2)
  z_power <- effect / se - z_crit
  power <- pnorm(z_power)
  return(power)
}

# Sample sizes to plot (per group)
n_seq <- seq(500, 10000, by = 100)

# Effect sizes (in percentage points)
effects_pp <- c(2, 2.5, 3, 3.5, 4) / 100

# Colors for different effect sizes
colors <- c("#E41A1C", "#FF7F00", "#4DAF4A", "#377EB8", "#984EA3")

# Create the plot
par(mar = c(5, 5, 4, 2))
plot(NULL, xlim = c(500, 10000), ylim = c(0, 1),
     xlab = "Sample Size (per treatment arm)",
     ylab = "Statistical Power",
     main = "Power Curves for GOTV Experiment\n(Baseline turnout = 40%, alpha = 0.05)",
     cex.lab = 1.2, cex.main = 1.1)

# Add grid
grid(col = "gray90")

# Plot power curves for each effect size
for (i in seq_along(effects_pp)) {
  powers <- sapply(n_seq, function(n) gotv_power(n, effects_pp[i]))
  lines(n_seq, powers, col = colors[i], lwd = 2.5)
}

# Add 80% power threshold
abline(h = 0.80, col = "gray40", lty = 2, lwd = 2)
text(9500, 0.83, "80% power", col = "gray40", cex = 0.9)

# Add vertical line at our planned sample size
abline(v = n_per_group, col = "gray40", lty = 3, lwd = 1.5)
text(n_per_group + 300, 0.15, paste0("n = ", n_per_group),
     col = "gray40", cex = 0.8, srt = 90)

# Legend
legend("bottomright",
       legend = paste0(effects_pp * 100, " pp effect"),
       col = colors, lwd = 2.5,
       title = "Effect Size",
       bg = "white", cex = 0.9)

cat("Plot created!\n\n")

# Print power at key sample sizes for the different effect sizes
cat("Power at n = ", format(n_per_group, big.mark = ","), " per group:\n", sep = "")
cat("Effect Size | Power\n")
cat("------------|-------\n")
for (i in seq_along(effects_pp)) {
  p <- gotv_power(n_per_group, effects_pp[i])
  cat(sprintf("   %.1f pp    | %.1f%%\n", effects_pp[i] * 100, p * 100))
}

cat("\nKey insight from the visualization:\n")
cat("  - With n = ", format(n_per_group, big.mark = ","), " per group, we have ~80% power for 3pp effect\n", sep = "")
cat("  - If true effect is only 2pp, power drops to ~50%\n")
cat("  - If true effect is 4pp, power exceeds 95%\n")
cat("  - Smaller effects require MUCH larger samples\n\n")

# Save figure for slides
cat("Saving figure for slides...\n")
pdf("../decks/figures/gotv_power_curves.pdf", width = 8, height = 6)

par(mar = c(5, 5, 4, 2))
plot(NULL, xlim = c(500, 10000), ylim = c(0, 1),
     xlab = "Sample Size (per treatment arm)",
     ylab = "Statistical Power",
     main = "Power Curves for GOTV Experiment\n(Baseline turnout = 40%, alpha = 0.05)",
     cex.lab = 1.2, cex.main = 1.1)

grid(col = "gray90")

for (i in seq_along(effects_pp)) {
  powers <- sapply(n_seq, function(n) gotv_power(n, effects_pp[i]))
  lines(n_seq, powers, col = colors[i], lwd = 2.5)
}

abline(h = 0.80, col = "gray40", lty = 2, lwd = 2)
text(9500, 0.83, "80% power", col = "gray40", cex = 0.9)

abline(v = n_per_group, col = "gray40", lty = 3, lwd = 1.5)
text(n_per_group + 300, 0.15, paste0("n = ", n_per_group),
     col = "gray40", cex = 0.8, srt = 90)

legend("bottomright",
       legend = paste0(effects_pp * 100, " pp effect"),
       col = colors, lwd = 2.5,
       title = "Effect Size",
       bg = "white", cex = 0.9)

dev.off()
cat("Saved to ../decks/figures/gotv_power_curves.pdf\n")

# -----------------------------------------------------------------------------
# Power Analysis: How Many Subjects Do I Need? (General Formula)
# -----------------------------------------------------------------------------

cat("\n=== Sample Size Calculation (General Formula) ===\n")
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
