# ============================================================================
# week13_section_code.R
# Gov 51 Section — Week 13: Exam 2 Review
# Simulations and plots for George's section deck
# ============================================================================
# Generates four PNG figures:
#   plot_selection_bias.png   — selection bias: Y(0) distributions differ
#   plot_overfitting.png      — train vs test RMSE as lambda increases
#   plot_ovb_simulation.png   — OLS vs 2SLS sampling distributions
#   plot_weak_iv.png          — 2SLS bias as a function of F-statistic
# ============================================================================

library(tidyverse)

set.seed(2001)

# ----------------------------------------------------------------------------
# SHARED THEME: George's palette
# ----------------------------------------------------------------------------
george_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F7FAFC", color = NA),
    panel.grid.major = element_line(color = "#CBD5E0", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#4A5568"),
    axis.title       = element_text(color = "#2D3748", face = "bold"),
    plot.title       = element_text(color = "#2D3748", face = "bold", size = 14),
    plot.subtitle    = element_text(color = "#718096", size = 11),
    legend.background = element_rect(fill = "white", color = "#CBD5E0"),
    legend.title     = element_text(face = "bold", color = "#2D3748"),
    legend.text      = element_text(color = "#4A5568"),
    strip.background = element_rect(fill = "#4A5568"),
    strip.text       = element_text(color = "white", face = "bold")
  )

george_colors <- c(
  slate   = "#4A5568",
  blue    = "#4299E1",
  amber   = "#D69E2E",
  teal    = "#319795",
  red     = "#E53E3E",
  purple  = "#805AD5",
  green   = "#48BB78"
)

# ============================================================================
# PLOT 1: SELECTION BIAS
# Y(0) distributions differ between treated and control groups
# Intuition: even without any treatment effect, the groups look different
# ============================================================================

n <- 2000

# Ability drives both selection into treatment and Y(0)
ability <- rnorm(n, mean = 0, sd = 1)

# Treatment: high ability -> more likely to be treated (college)
prob_treat <- plogis(0.8 * ability)
D <- rbinom(n, 1, prob_treat)

# Potential outcomes: ability raises Y(0) for everyone
Y0 <- 10 + 3 * ability + rnorm(n, 0, 1)
Y1 <- Y0 + 2    # true treatment effect = 2 (same for everyone)
Y_obs <- ifelse(D == 1, Y1, Y0)

df1 <- tibble(
  D     = factor(D, labels = c("Control (D=0)", "Treated (D=1)")),
  Y0    = Y0,
  Y_obs = Y_obs
)

# Show Y(0) distributions for treated vs control
p1 <- df1 |>
  ggplot(aes(x = Y0, fill = D, color = D)) +
  geom_density(alpha = 0.35, linewidth = 1) +
  scale_fill_manual(values  = c(george_colors["blue"],  george_colors["amber"]),
                    name = NULL) +
  scale_color_manual(values = c(george_colors["blue"],  george_colors["amber"]),
                     name = NULL) +
  geom_vline(
    data = df1 |> group_by(D) |> summarize(m = mean(Y0)),
    aes(xintercept = m, color = D), linewidth = 1.2, linetype = "dashed"
  ) +
  labs(
    title    = "Selection bias: Y(0) is higher for the treated group",
    subtitle = "Dashed lines = group means of Y(0). The gap is selection bias.",
    x        = expression(Y[i](0) ~ " — untreated potential outcome"),
    y        = "Density"
  ) +
  annotate("segment",
    x = 11, xend = 13, y = 0.34, yend = 0.34,
    arrow = arrow(length = unit(0.15, "cm"), ends = "both"),
    color = george_colors["red"], linewidth = 1
  ) +
  annotate("text",
    x = 12, y = 0.37, label = "Selection bias",
    color = george_colors["red"], fontface = "bold", size = 3.8
  ) +
  george_theme +
  theme(legend.position = c(0.18, 0.85))

ggsave("plot_selection_bias.png", p1, width = 8, height = 4.5, dpi = 150)
cat("Saved plot_selection_bias.png\n")

# ============================================================================
# PLOT 2: OVERFITTING — TRAIN VS TEST RMSE
# Simulation: polynomial regression on noisy data
# ============================================================================

set.seed(42)
n_train <- 80
n_test  <- 200

# True data-generating process: quadratic
x_train <- runif(n_train, -3, 3)
y_train <- 2 + 0.5 * x_train - 0.3 * x_train^2 + rnorm(n_train, 0, 1.5)

x_test  <- runif(n_test, -3, 3)
y_test  <- 2 + 0.5 * x_test  - 0.3 * x_test^2  + rnorm(n_test,  0, 1.5)

# Fit polynomials of degree 1 through 12
degrees <- 1:12
results <- map_dfr(degrees, function(d) {
  fit <- lm(y_train ~ poly(x_train, d, raw = TRUE))
  y_hat_train <- predict(fit)
  y_hat_test  <- predict(fit, newdata = data.frame(x_train = x_test))
  tibble(
    degree     = d,
    rmse_train = sqrt(mean((y_hat_train - y_train)^2)),
    rmse_test  = sqrt(mean((y_hat_test  - y_test)^2))
  )
})

p2 <- results |>
  pivot_longer(cols = c(rmse_train, rmse_test),
               names_to = "split", values_to = "rmse") |>
  mutate(split = recode(split,
                        rmse_train = "Train RMSE",
                        rmse_test  = "Test RMSE")) |>
  ggplot(aes(x = degree, y = rmse, color = split, group = split)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2, linetype = "dashed",
             color = george_colors["teal"], linewidth = 1) +
  annotate("text", x = 2.3, y = 3.2,
           label = "True model\n(degree 2)", hjust = 0,
           color = george_colors["teal"], fontface = "bold", size = 3.5) +
  scale_color_manual(values = c("Train RMSE" = george_colors["blue"],
                                "Test RMSE"  = george_colors["red"]),
                     name = NULL) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title    = "Overfitting: train RMSE keeps falling; test RMSE rises",
    subtitle = "Adding polynomial terms always improves in-sample fit — but hurts out-of-sample",
    x        = "Polynomial degree (model complexity)",
    y        = "RMSE"
  ) +
  george_theme +
  theme(legend.position = c(0.80, 0.80))

ggsave("plot_overfitting.png", p2, width = 8, height = 4.5, dpi = 150)
cat("Saved plot_overfitting.png\n")

# ============================================================================
# PLOT 3: OLS VS 2SLS SAMPLING DISTRIBUTIONS
# Simulation: repeat 2000 times, show OLS is biased, IV is centered on truth
# ============================================================================

set.seed(8675)
n_obs  <- 500
n_reps <- 2000
beta_true <- 0.5   # true causal effect

run_sim <- function(strength) {
  map_dfr(1:n_reps, function(i) {
    u <- rnorm(n_obs)                           # unobservable confounder
    Z <- rnorm(n_obs)                           # instrument
    D <- strength * Z + 0.8 * u + rnorm(n_obs) # first stage; u creates endogeneity
    Y <- beta_true * D + u + rnorm(n_obs)       # structural equation

    # OLS
    ols  <- coef(lm(Y ~ D))["D"]

    # 2SLS (Wald)
    D_hat <- fitted(lm(D ~ Z))
    tsls  <- coef(lm(Y ~ D_hat))["D_hat"]

    tibble(ols = ols, tsls = tsls)
  })
}

strong_sim <- run_sim(strength = 2.0)

df3 <- bind_rows(
  strong_sim |> select(est = ols)  |> mutate(estimator = "OLS"),
  strong_sim |> select(est = tsls) |> mutate(estimator = "2SLS")
)

p3 <- df3 |>
  ggplot(aes(x = est, fill = estimator, color = estimator)) +
  geom_density(alpha = 0.35, linewidth = 1) +
  geom_vline(xintercept = beta_true, linewidth = 1.4,
             color = george_colors["teal"], linetype = "dashed") +
  annotate("text", x = beta_true + 0.02, y = 12,
           label = expression(beta[true] == 0.5),
           color = george_colors["teal"], hjust = 0, fontface = "bold", size = 4) +
  scale_fill_manual(values  = c("OLS"  = george_colors["red"],
                                "2SLS" = george_colors["blue"]),
                    name = NULL) +
  scale_color_manual(values = c("OLS"  = george_colors["red"],
                                "2SLS" = george_colors["blue"]),
                     name = NULL) +
  labs(
    title    = "2SLS is centered on the truth; OLS is not",
    subtitle = "2,000 simulation draws, n=500. Instrument is strong (F ≈ 100).",
    x        = "Estimated coefficient",
    y        = "Density"
  ) +
  george_theme +
  theme(legend.position = c(0.15, 0.85))

ggsave("plot_ovb_simulation.png", p3, width = 8, height = 4.5, dpi = 150)
cat("Saved plot_ovb_simulation.png\n")

# ============================================================================
# PLOT 4: WEAK IV — 2SLS SAMPLING DISTRIBUTION SLIDES TOWARD OLS AS F FALLS
# Show three instrument strengths: strong, moderate, weak
# ============================================================================

set.seed(314)
n_obs  <- 300
n_reps <- 3000
beta_true <- 0.5

sim_by_strength <- function(strength_val, label) {
  map_dbl(1:n_reps, function(i) {
    u <- rnorm(n_obs)
    Z <- rnorm(n_obs)
    D <- strength_val * Z + 0.9 * u + rnorm(n_obs)
    Y <- beta_true * D + u + rnorm(n_obs)
    D_hat <- fitted(lm(D ~ Z))
    coef(lm(Y ~ D_hat))["D_hat"]
  }) |>
    tibble(est = _, strength = label)
}

df4 <- bind_rows(
  sim_by_strength(2.5,  "Strong  (F ≈ 130)"),
  sim_by_strength(0.6,  "Moderate (F ≈ 10)"),
  sim_by_strength(0.18, "Weak    (F ≈ 1)")
)

# Also get OLS bias reference
ols_bias_ref <- mean(map_dbl(1:n_reps, function(i) {
  u <- rnorm(n_obs); Z <- rnorm(n_obs)
  D <- 2.5 * Z + 0.9 * u + rnorm(n_obs)
  Y <- beta_true * D + u + rnorm(n_obs)
  coef(lm(Y ~ D))["D"]
}))

df4 <- df4 |>
  mutate(strength = factor(strength,
    levels = c("Strong  (F ≈ 130)", "Moderate (F ≈ 10)", "Weak    (F ≈ 1)")))

p4 <- df4 |>
  ggplot(aes(x = est, color = strength, fill = strength)) +
  geom_density(alpha = 0.25, linewidth = 1.2) +
  geom_vline(xintercept = beta_true, linewidth = 1.4,
             color = george_colors["teal"], linetype = "dashed") +
  geom_vline(xintercept = ols_bias_ref, linewidth = 1.2,
             color = george_colors["red"], linetype = "dotted") +
  annotate("text", x = beta_true - 0.03, y = 5.5,
           label = expression(beta[true]), hjust = 1,
           color = george_colors["teal"], fontface = "bold", size = 4) +
  annotate("text", x = ols_bias_ref + 0.03, y = 5.5,
           label = "OLS bias", hjust = 0,
           color = george_colors["red"], fontface = "bold", size = 4) +
  scale_fill_manual(values  = c(george_colors["blue"],
                                george_colors["amber"],
                                george_colors["red"]),
                    name = "Instrument") +
  scale_color_manual(values = c(george_colors["blue"],
                                george_colors["amber"],
                                george_colors["red"]),
                     name = "Instrument") +
  coord_cartesian(xlim = c(-1, 3)) +
  labs(
    title    = "Weak instruments: 2SLS slides toward OLS bias",
    subtitle = "3,000 simulation draws, n=300. As F falls, bias grows and variance explodes.",
    x        = "2SLS estimate",
    y        = "Density"
  ) +
  george_theme +
  theme(legend.position = c(0.75, 0.78))

ggsave("plot_weak_iv.png", p4, width = 8, height = 4.5, dpi = 150)
cat("Saved plot_weak_iv.png\n")

cat("\nAll plots generated. Open the PNGs in the week13 directory.\n")
