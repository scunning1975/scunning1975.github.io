# week06b_bootstrap.R
# Simulate the sampling distribution of beta_1 from wage1
# Shows students what "different samples → different estimates" looks like

library(wooldridge)
library(tidyverse)

set.seed(42)
data(wage1)

# Our actual estimate
m1 <- lm(wage ~ female, data = wage1)
our_beta <- coef(m1)["female"]  # -2.5118

# Bootstrap: resample 526 workers with replacement, 1000 times
n_boot <- 1000
boot_betas <- replicate(n_boot, {
  boot_sample <- wage1[sample(nrow(wage1), replace = TRUE), ]
  coef(lm(wage ~ female, data = boot_sample))["female"]
})

cat("Bootstrap results:\n")
cat("  Mean of beta_1 hats:", round(mean(boot_betas), 4), "\n")
cat("  SD of beta_1 hats (≈ SE):", round(sd(boot_betas), 4), "\n")
cat("  Our actual beta_1:", round(our_beta, 4), "\n")
cat("  Analytical SE:", round(summary(m1)$coefficients[2, 2], 4), "\n")

# Colors
crimson <- "#A51C30"
navy <- "#1E3C72"
blue <- "#2980B9"
gray_col <- "#7F8C8D"

theme_gov51 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = navy, face = "bold", size = 16),
    plot.subtitle = element_text(color = gray_col, size = 12),
    axis.title = element_text(color = navy),
    panel.grid.minor = element_blank()
  )

# Figure: Sampling distribution histogram
p <- ggplot(data.frame(beta = boot_betas), aes(x = beta)) +
  geom_histogram(bins = 40, fill = blue, alpha = 0.6, color = "white") +
  geom_vline(xintercept = our_beta, color = crimson, linewidth = 1.5,
             linetype = "solid") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  annotate("text", x = our_beta - 0.08, y = Inf, label = "Our sample",
           color = crimson, fontface = "bold", size = 5, vjust = 2, hjust = 1) +
  annotate("text", x = 0.08, y = Inf, label = "Zero",
           color = "black", size = 4.5, vjust = 2, hjust = 0) +
  labs(x = expression(hat(beta)[1]),
       y = "Count",
       title = "1,000 different samples from the same population") +
  scale_x_continuous(breaks = seq(-4, 1, by = 0.5)) +
  theme_gov51

ggsave("figures/sampling_distribution.pdf", p, width = 9, height = 5)

cat("\nFigure saved: figures/sampling_distribution.pdf\n")

# Figure 2: Density curve with SD bands
boot_mean <- mean(boot_betas)
boot_sd <- sd(boot_betas)

p2 <- ggplot(data.frame(beta = boot_betas), aes(x = beta)) +
  # ±2 SD shading (lighter)
  annotate("rect",
           xmin = boot_mean - 2 * boot_sd, xmax = boot_mean + 2 * boot_sd,
           ymin = -Inf, ymax = Inf,
           fill = blue, alpha = 0.10) +
  # ±1 SD shading (darker)
  annotate("rect",
           xmin = boot_mean - 1 * boot_sd, xmax = boot_mean + 1 * boot_sd,
           ymin = -Inf, ymax = Inf,
           fill = blue, alpha = 0.15) +
  # Density curve
  geom_density(fill = blue, alpha = 0.3, color = navy, linewidth = 1) +
  # Center line
  geom_vline(xintercept = boot_mean, color = navy, linewidth = 1.2,
             linetype = "solid") +
  # ±1 SD bracket labels
  annotate("segment", x = boot_mean - boot_sd, xend = boot_mean + boot_sd,
           y = 0.05, yend = 0.05, color = crimson, linewidth = 1.2,
           arrow = arrow(ends = "both", length = unit(0.08, "inches"))) +
  annotate("text", x = boot_mean, y = 0.08,
           label = paste0("1 SD = ", round(boot_sd, 2), " ~ SE"),
           color = crimson, fontface = "bold", size = 5) +
  # ±2 SD bracket labels
  annotate("segment", x = boot_mean - 2*boot_sd, xend = boot_mean + 2*boot_sd,
           y = -0.03, yend = -0.03, color = navy, linewidth = 0.8,
           arrow = arrow(ends = "both", length = unit(0.06, "inches"))) +
  annotate("text", x = boot_mean, y = -0.06,
           label = "2 SD -- 95% of samples fall here",
           color = navy, fontface = "bold", size = 4.5) +
  labs(x = expression(hat(beta)[1] ~ "($/hr)"),
       y = "",
       title = expression("Sampling distribution of " * hat(beta)[1])) +
  scale_x_continuous(breaks = seq(-4, -1, by = 0.5)) +
  theme_gov51 +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("figures/beta_sampling_dist.pdf", p2, width = 9, height = 5)

cat("Figure saved: figures/beta_sampling_dist.pdf\n")
