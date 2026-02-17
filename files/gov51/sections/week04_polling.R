# ============================================================================
# week04_polling.R
# Gov 51 Section - Week 4: Correlation and Sampling
# George's R Demo Script
# February 18, 2026
# ============================================================================

library(tidyverse)

# ============================================================================
# PART 1: CORRELATION BY HAND
# ============================================================================

# The exercise data
x <- c(2, 4, 6, 8, 10)
y <- c(3, 5, 4, 8, 9)

# Step 1: Means
x_bar <- mean(x)  # 6
y_bar <- mean(y)  # 5.8

# Step 2: Deviations
x_dev <- x - x_bar
y_dev <- y - y_bar

# Step 3: Products of deviations
products <- x_dev * y_dev

# Show the full table
tibble(
  i = 1:5,
  x = x,
  y = y,
  `x - x_bar` = x_dev,
  `y - y_bar` = y_dev,
  `product` = products
)

# Step 4: Covariance = sum of products / (n-1)
cov_manual <- sum(products) / (length(x) - 1)
cat("Covariance (manual):", cov_manual, "\n")
cat("Covariance (R):     ", cov(x, y), "\n")

# Step 5: Standard deviations
sd_x <- sd(x)  # sqrt(10) = 3.162
sd_y <- sd(y)  # sqrt(6.7) = 2.588

# Step 6: Correlation = cov / (sd_x * sd_y)
r_manual <- cov_manual / (sd_x * sd_y)
cat("\nCorrelation (manual):", round(r_manual, 4), "\n")
cat("Correlation (R):     ", round(cor(x, y), 4), "\n")


# ============================================================================
# PART 2: ANSCOMBE'S QUARTET
# ============================================================================

# R has Anscombe's data built in
data(anscombe)

# All four datasets have the same correlation!
cat("\nAnscombe's Quartet Correlations:\n")
cat("Dataset I:  ", round(cor(anscombe$x1, anscombe$y1), 3), "\n")
cat("Dataset II: ", round(cor(anscombe$x2, anscombe$y2), 3), "\n")
cat("Dataset III:", round(cor(anscombe$x3, anscombe$y3), 3), "\n")
cat("Dataset IV: ", round(cor(anscombe$x4, anscombe$y4), 3), "\n")

# Plot all four
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

plot(anscombe$x1, anscombe$y1, pch = 19, col = "steelblue",
     main = "I: Linear", xlab = "x", ylab = "y")
abline(lm(y1 ~ x1, data = anscombe), col = "gray40", lty = 2)

plot(anscombe$x2, anscombe$y2, pch = 19, col = "darkcyan",
     main = "II: Curved", xlab = "x", ylab = "y")
abline(lm(y2 ~ x2, data = anscombe), col = "gray40", lty = 2)

plot(anscombe$x3, anscombe$y3, pch = 19, col = "goldenrod",
     main = "III: Outlier", xlab = "x", ylab = "y")
abline(lm(y3 ~ x3, data = anscombe), col = "gray40", lty = 2)

plot(anscombe$x4, anscombe$y4, pch = 19, col = "mediumpurple",
     main = "IV: Vertical", xlab = "x", ylab = "y")
abline(lm(y4 ~ x4, data = anscombe), col = "gray40", lty = 2)

par(mfrow = c(1, 1))  # Reset layout


# ============================================================================
# PART 3: SE, MOE, AND CONFIDENCE INTERVALS
# ============================================================================

# Example 1: n = 900, p_hat = 0.54
p_hat <- 0.54
n <- 900

se <- sqrt(p_hat * (1 - p_hat) / n)
moe <- 1.96 * se
ci_lower <- p_hat - moe
ci_upper <- p_hat + moe

cat("\n--- Example 1: n = 900, p_hat = 0.54 ---\n")
cat("SE:  ", round(se, 4), "\n")
cat("MOE: ", round(moe, 4), "\n")
cat("95% CI: [", round(ci_lower, 3), ",", round(ci_upper, 3), "]\n")

# Example 2 (practice problem): n = 1200, p_hat = 0.47
p_hat2 <- 0.47
n2 <- 1200

se2 <- sqrt(p_hat2 * (1 - p_hat2) / n2)
moe2 <- 1.96 * se2
ci_lower2 <- p_hat2 - moe2
ci_upper2 <- p_hat2 + moe2

cat("\n--- Example 2: n = 1200, p_hat = 0.47 ---\n")
cat("SE:  ", round(se2, 4), "\n")
cat("MOE: ", round(moe2, 4), "\n")
cat("95% CI: [", round(ci_lower2, 3), ",", round(ci_upper2, 3), "]\n")
cat("Is 50% in the CI?", ci_lower2 <= 0.50 & ci_upper2 >= 0.50, "\n")


# ============================================================================
# PART 4: FLORIDA POLLS (2008 ELECTION)
# ============================================================================

polls <- read_csv("polls08.csv")

cat("\n--- 2008 Polling Data ---\n")
cat("Total polls:", nrow(polls), "\n")
cat("States:", n_distinct(polls$state), "\n")

# Focus on Florida
florida <- polls |> filter(state == "FL")

cat("\n--- Florida ---\n")
cat("Number of polls:", nrow(florida), "\n")
cat("Mean Obama %:   ", round(mean(florida$Obama), 1), "\n")
cat("SD:             ", round(sd(florida$Obama), 1), "\n")
cat("Range:          ", min(florida$Obama), "to", max(florida$Obama), "\n")

# Calculate SE and CI for Florida
p_fl <- mean(florida$Obama) / 100  # Convert to proportion
n_typical <- 1000  # Typical poll sample size

se_fl <- sqrt(p_fl * (1 - p_fl) / n_typical)
ci_fl_lower <- p_fl - 1.96 * se_fl
ci_fl_upper <- p_fl + 1.96 * se_fl

cat("\nUsing n = 1000 (typical poll):\n")
cat("SE:     ", round(se_fl, 4), "\n")
cat("95% CI: [", round(ci_fl_lower * 100, 1), "%,",
    round(ci_fl_upper * 100, 1), "%]\n")
cat("Obama actually won FL with ~51%\n")
