# =============================================================================
# Gov 51: Descriptive Statistics
# Week 2, Thursday - February 6, 2026
# =============================================================================
#
# This script accompanies the lecture on descriptive statistics.
# We'll use state-level presidential approval data to learn about:
#   - Measures of center (mean, median)
#   - Measures of spread (range, percentiles, variance, standard deviation)
#   - Weighted statistics
#   - Visualizing distributions
#
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------

# Load packages
library(tidyverse)

# Load data (state-level presidential approval ratings)
# Source: Simulated data based on typical patterns
url <- "https://raw.githubusercontent.com/scunning1975/scunning1975.github.io/master/files/gov51/data/state_approval.csv"
approval <- read.csv(url)

# Quick look at the data
dim(approval)        # 50 states, 5 variables
head(approval)       # First few rows
names(approval)      # Variable names


# -----------------------------------------------------------------------------
# MEASURES OF CENTER
# -----------------------------------------------------------------------------

# The mean: sum divided by count
mean(approval$approval)

# Calculate by hand to see what's happening:
sum(approval$approval) / length(approval$approval)

# The median: middle value when sorted
median(approval$approval)

# See the sorted values
sort(approval$approval)

# Compare mean and median
# When mean > median: right-skewed (pulled up by high values)
# When mean < median: left-skewed (pulled down by low values)
# When mean ≈ median: roughly symmetric


# -----------------------------------------------------------------------------
# MEASURES OF SPREAD
# -----------------------------------------------------------------------------

# Range: minimum and maximum
min(approval$approval)
max(approval$approval)
range(approval$approval)

# Percentiles (quantiles)
quantile(approval$approval)              # Default: 0%, 25%, 50%, 75%, 100%
quantile(approval$approval, 0.90)        # 90th percentile
quantile(approval$approval, c(0.10, 0.50, 0.90))  # Multiple percentiles

# Interquartile range (IQR): middle 50%
IQR(approval$approval)   # Same as Q3 - Q1

# Variance: average squared deviation from mean
var(approval$approval)

# Standard deviation: square root of variance (in original units)
sd(approval$approval)

# The summary() shortcut
summary(approval$approval)


# -----------------------------------------------------------------------------
# WEIGHTED STATISTICS
# -----------------------------------------------------------------------------

# Unweighted mean: each state counts equally
mean(approval$approval)

# Weighted mean: weight by population (each person counts equally)
weighted.mean(approval$approval, approval$population)

# The weighted mean is higher because large, high-approval states
# (California, New York) pull it up

# Compare:
# - Unweighted: "What does the typical STATE think?"
# - Weighted: "What does the typical PERSON think?"


# -----------------------------------------------------------------------------
# VISUALIZING DISTRIBUTIONS
# -----------------------------------------------------------------------------

# Basic histogram
ggplot(approval, aes(x = approval)) +
  geom_histogram(binwidth = 4, fill = "steelblue", color = "white") +
  labs(x = "Approval Rating (%)",
       y = "Number of States",
       title = "Distribution of Presidential Approval by State")

# Try different bin widths to see how it affects the picture
ggplot(approval, aes(x = approval)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Narrower bins (binwidth = 2)")

ggplot(approval, aes(x = approval)) +
  geom_histogram(binwidth = 8, fill = "steelblue", color = "white") +
  labs(title = "Wider bins (binwidth = 8)")

# Add mean and median lines
ggplot(approval, aes(x = approval)) +
  geom_histogram(binwidth = 4, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean(approval$approval),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(approval$approval),
             color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(x = "Approval Rating (%)",
       y = "Number of States",
       title = "Distribution with Mean (red) and Median (green)")


# -----------------------------------------------------------------------------
# BUILDING A SUMMARY STATISTICS TABLE
# -----------------------------------------------------------------------------

# Calculate all statistics
stats <- data.frame(
  Variable = c("Approval (%)", "Population (millions)"),
  N = c(length(approval$approval),
        length(approval$population)),
  Mean = c(mean(approval$approval),
           mean(approval$population) / 1e6),
  SD = c(sd(approval$approval),
         sd(approval$population) / 1e6),
  Min = c(min(approval$approval),
          min(approval$population) / 1e6),
  Max = c(max(approval$approval),
          max(approval$population) / 1e6)
)

# View the table
stats

# Round for cleaner display
stats_rounded <- stats
stats_rounded[, -1] <- round(stats[, -1], 2)
stats_rounded

# In Quarto/RMarkdown, use kable() for nice formatting:
# knitr::kable(stats_rounded, digits = 2)


# -----------------------------------------------------------------------------
# PRACTICE: REGIONAL DIFFERENCES
# -----------------------------------------------------------------------------

# What's the mean approval by region?
approval %>%
  group_by(region) %>%
  summarize(
    n_states = n(),
    mean_approval = mean(approval),
    median_approval = median(approval),
    sd_approval = sd(approval)
  )

# Visualize by region
ggplot(approval, aes(x = approval, fill = region)) +
  geom_histogram(binwidth = 4, color = "white", alpha = 0.7) +
  facet_wrap(~region) +
  labs(x = "Approval Rating (%)",
       y = "Number of States",
       title = "Approval by Region") +
  theme(legend.position = "none")


# -----------------------------------------------------------------------------
# KEY TAKEAWAYS
# -----------------------------------------------------------------------------

# 1. MEAN vs MEDIAN
#    - Mean uses all values, sensitive to outliers
#    - Median uses only the middle, robust to outliers
#    - Compare them to understand skewness

# 2. SPREAD
#    - Range: simple but sensitive to outliers
#    - Standard deviation: average distance from mean
#    - Percentiles: where values fall in the distribution

# 3. WEIGHTED STATISTICS
#    - Use when observations represent different amounts
#    - weighted.mean(values, weights)

# 4. VISUALIZATION
#    - Histograms show shape, center, spread
#    - Experiment with bin width
#    - Always describe what you see!

# =============================================================================
# END OF SCRIPT
# =============================================================================
