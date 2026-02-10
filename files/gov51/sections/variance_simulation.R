# variance_simulation.R
# Demonstrates why we use n-1 (Bessel's correction) for sample variance
# Gov 51 Section - Week 3
#
# Run this script to see visual proof that:
# - Dividing by n underestimates the true variance
# - Dividing by n-1 gives an unbiased estimate

library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# ============================================================================
# STEP 1: Create a "population" with KNOWN variance
# ============================================================================

# True population parameters
true_mean <- 50
true_sd <- 10
true_variance <- true_sd^2  # = 100

# Create a large population
population <- rnorm(100000, mean = true_mean, sd = true_sd)

cat("True population variance:", true_variance, "\n")
cat("Actual population variance:", round(var(population), 2), "\n\n")

# ============================================================================
# STEP 2: Draw many samples and calculate variance two ways
# ============================================================================

n_samples <- 10000    # Number of samples to draw
sample_size <- 30     # Size of each sample

# Store results
results <- tibble(sample_id = 1:n_samples) %>%
  rowwise() %>%
  mutate(
    # Draw a sample
    sample_data = list(sample(population, sample_size)),

    # Calculate variance with 1/n (WRONG way)
    var_n = sum((sample_data - mean(sample_data))^2) / sample_size,

    # Calculate variance with 1/(n-1) (CORRECT way - Bessel's correction)
    var_n1 = sum((sample_data - mean(sample_data))^2) / (sample_size - 1)
  ) %>%
  ungroup() %>%
  select(-sample_data)

# ============================================================================
# STEP 3: Compare the averages
# ============================================================================

cat("=== RESULTS ===\n")
cat("True population variance:        ", true_variance, "\n")
cat("Average of 1/n estimates:        ", round(mean(results$var_n), 2), "\n")
cat("Average of 1/(n-1) estimates:    ", round(mean(results$var_n1), 2), "\n\n")

# Calculate bias
bias_n <- mean(results$var_n) - true_variance
bias_n1 <- mean(results$var_n1) - true_variance

cat("Bias using 1/n:    ", round(bias_n, 2), " (underestimates!)\n")
cat("Bias using 1/(n-1):", round(bias_n1, 2), " (approximately unbiased)\n\n")

# ============================================================================
# STEP 4: Visualize the distributions
# ============================================================================

# Reshape for plotting
plot_data <- results %>%
  pivot_longer(
    cols = c(var_n, var_n1),
    names_to = "method",
    values_to = "estimate"
  ) %>%
  mutate(
    method = case_when(
      method == "var_n" ~ "Divide by n (biased)",
      method == "var_n1" ~ "Divide by n-1 (unbiased)"
    )
  )

# Create the plot
p <- ggplot(plot_data, aes(x = estimate, fill = method)) +
  geom_histogram(
    alpha = 0.6,
    position = "identity",
    bins = 50,
    color = "white"
  ) +
  geom_vline(
    xintercept = true_variance,
    linetype = "dashed",
    linewidth = 1,
    color = "#2D3748"
  ) +
  annotate(
    "text",
    x = true_variance + 2,
    y = Inf,
    label = paste("True variance =", true_variance),
    hjust = 0,
    vjust = 2,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Divide by n (biased)" = "#E53E3E",
      "Divide by n-1 (unbiased)" = "#4299E1"
    )
  ) +
  labs(
    title = "Why We Divide by n-1 for Sample Variance",
    subtitle = paste0(
      "Based on ", format(n_samples, big.mark = ","),
      " samples of size ", sample_size,
      " from a population with variance = ", true_variance
    ),
    x = "Variance Estimate",
    y = "Count",
    fill = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40")
  )

# Display the plot
print(p)

# Save the plot
ggsave(
  "variance_simulation_plot.png",
  p,
  width = 10,
  height = 6,
  dpi = 150
)

cat("Plot saved to: variance_simulation_plot.png\n")

# ============================================================================
# STEP 5: Summary table
# ============================================================================

summary_table <- tibble(
  Method = c("Divide by n", "Divide by n-1"),
  `Average Estimate` = c(mean(results$var_n), mean(results$var_n1)),
  `Bias` = c(bias_n, bias_n1),
  `Standard Error` = c(sd(results$var_n), sd(results$var_n1))
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

cat("\n=== SUMMARY TABLE ===\n")
print(summary_table)

# ============================================================================
# TAKEAWAY
# ============================================================================

cat("\n")
cat("=== KEY TAKEAWAY ===\n")
cat("When we use 1/n, we systematically underestimate the true variance.\n")
cat("Dividing by n-1 (Bessel's correction) fixes this bias.\n")
cat("This is why R's var() and sd() functions use n-1 by default.\n")
