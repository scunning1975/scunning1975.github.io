# ============================================================================
# Gov 51 Section — Week 11
# Weight Loss Pill Simulation: Selection Bias vs. Randomization
# ============================================================================
#
# This script is the replication file for the simulation section of
# week11_section.tex. Run from the week11/ directory or open in RStudio.
#
# Produces:
#   wt_po_density.pdf       -- Y0/Y1 potential outcome distributions
#   wt_love_plot.pdf        -- covariate balance love plot (both scenarios)
#   wt_means_compare.pdf    -- group means: SDO vs ATE (both scenarios)
#
# Packages: tidyverse, patchwork, scales
# ============================================================================

library(tidyverse)
library(patchwork)
library(scales)

# Output path: current directory when running from week11/
out_path <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "."
)

# George-style color palette (matches section deck)
col_trt  <- "#4299E1"   # treated:  blue
col_ctrl <- "#D69E2E"   # control:  amber
col_ate  <- "#319795"   # ATE:      teal
col_sdo  <- "#E53E3E"   # SDO:      red
col_y0   <- "#4299E1"   # Y0 curve: blue
col_y1   <- "#E53E3E"   # Y1 curve: red

# ============================================================================
# STEP 1: Generate 10,000 people
# ============================================================================

set.seed(51)
N <- 10000

sim <- tibble(
  age     = round(rnorm(N, 45, 12)),         # age in years
  female  = rbinom(N, 1, 0.52),              # 1 = female
  poc     = rbinom(N, 1, 0.35),              # 1 = person of color
  college = rbinom(N, 1, 0.45),              # 1 = college educated
  # Baseline weight: males 18 lbs heavier on average; slight age gradient
  Y0 = round(185 + 18*(1 - female) + 0.3*(age - 45) + rnorm(N, 0, 28))
)

cat("=== Population summary ===\n")
cat(sprintf("N = %d\n", N))
cat(sprintf("Mean age:     %.1f years\n", mean(sim$age)))
cat(sprintf("Pct female:   %.1f%%\n",     mean(sim$female)*100))
cat(sprintf("Pct POC:      %.1f%%\n",     mean(sim$poc)*100))
cat(sprintf("Pct college:  %.1f%%\n",     mean(sim$college)*100))
cat(sprintf("Mean weight:  %.1f lbs\n",   mean(sim$Y0)))

# ============================================================================
# STEP 2: Generate potential outcomes (constant treatment effect)
# ============================================================================

sim <- sim |>
  mutate(
    delta = -15,         # pill reduces weight by exactly 15 lbs for everyone
    Y1    = Y0 + delta   # Y1 = Y0 - 15
  )

ATE     <- mean(sim$delta)    # = -15
mean_Y0 <- mean(sim$Y0)       # ≈ 194 lbs

cat(sprintf("\nTrue ATE:  %d lbs\n", ATE))
cat(sprintf("Mean Y0:   %.1f lbs\n", mean_Y0))
cat(sprintf("Mean Y1:   %.1f lbs\n", mean(sim$Y1)))

# ============================================================================
# FIGURE 1: Distribution of Y0 and Y1
# ============================================================================

po_long <- sim |>
  select(Y0, Y1) |>
  pivot_longer(everything(), names_to = "po", values_to = "weight") |>
  mutate(po = factor(po, levels = c("Y0", "Y1"),
                     labels = c("Y\u2080: no pill", "Y\u2081: takes pill")))

means_po <- tibble(
  po     = factor(c("Y\u2080: no pill", "Y\u2081: takes pill"),
                  levels = c("Y\u2080: no pill", "Y\u2081: takes pill")),
  mean_w = c(mean_Y0, mean_Y0 + ATE)
)

arrow_y <- 0.0185

p_po <- ggplot(po_long, aes(x = weight, fill = po, color = po)) +
  geom_density(alpha = 0.28, linewidth = 1.0) +
  geom_vline(data = means_po,
             aes(xintercept = mean_w, color = po),
             linetype = "dashed", linewidth = 0.9, show.legend = FALSE) +
  annotate("segment",
           x    = mean_Y0 + ATE + 1, xend = mean_Y0 - 1,
           y    = arrow_y, yend = arrow_y,
           arrow = arrow(length = unit(0.22, "cm"), ends = "both"),
           color = col_ate, linewidth = 1.1) +
  annotate("text",
           x = mean_Y0 + ATE/2, y = arrow_y + 0.0018,
           label = "ATE = \u221215 lbs",
           color = col_ate, size = 3.8, fontface = "bold") +
  scale_fill_manual(values  = c("Y\u2080: no pill" = col_y0,
                                 "Y\u2081: takes pill" = col_y1)) +
  scale_color_manual(values = c("Y\u2080: no pill" = col_y0,
                                 "Y\u2081: takes pill" = col_y1)) +
  scale_x_continuous(breaks = seq(100, 310, 30)) +
  coord_cartesian(xlim = c(80, 320)) +
  labs(x = "Weight (lbs)", y = "Density",
       fill = NULL, color = NULL,
       title = "Potential outcomes: Y\u2081 is just Y\u2080 shifted 15 lbs to the left") +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "top",
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold", size = 11.5))

ggsave(file.path(out_path, "wt_po_density.pdf"),
       p_po, width = 7.5, height = 3.5, device = cairo_pdf)
message("wt_po_density.pdf written")

# ============================================================================
# STEP 3: Two treatment assignment scenarios
# ============================================================================

sim <- sim |>
  mutate(
    # A: heavy people choose the pill
    D_A   = as.integer(Y0 > mean_Y0),
    # B: random assignment
    D_B   = rbinom(N, 1, 0.5),
    # Switching equation: Y_obs = D*Y1 + (1-D)*Y0
    Y_A   = D_A * Y1 + (1 - D_A) * Y0,
    Y_B   = D_B * Y1 + (1 - D_B) * Y0
  )

# ============================================================================
# STEP 4: Compute estimands
# ============================================================================

# Scenario A: Selection
E_Y0_D1_A <- mean(sim$Y0[sim$D_A == 1])
E_Y0_D0_A <- mean(sim$Y0[sim$D_A == 0])
SB_A      <- E_Y0_D1_A - E_Y0_D0_A
SDO_A     <- mean(sim$Y_A[sim$D_A == 1]) - mean(sim$Y_A[sim$D_A == 0])

cat("\n=== Scenario A: Selection (D = 1 if Y0 > mean_Y0) ===\n")
cat(sprintf("Fraction treated:  %.1f%%\n", mean(sim$D_A)*100))
cat(sprintf("E[Y0 | D=1]:  %.1f lbs  (heavier people chose pill)\n", E_Y0_D1_A))
cat(sprintf("E[Y0 | D=0]:  %.1f lbs\n", E_Y0_D0_A))
cat(sprintf("Selection bias:   +%.1f lbs\n", SB_A))
cat(sprintf("ATE:              %d  lbs\n", ATE))
cat(sprintf("SDO:              +%.1f lbs  <-- biased!\n", SDO_A))
cat(sprintf("ATE + SB:         %.1f lbs  (= SDO: decomposition checks out)\n", ATE + SB_A))
fit_A <- lm(Y_A ~ D_A, data = sim)
cat(sprintf("OLS coef on D_A:  %.2f lbs\n", coef(fit_A)["D_A"]))

# Scenario B: Randomization
E_Y0_D1_B <- mean(sim$Y0[sim$D_B == 1])
E_Y0_D0_B <- mean(sim$Y0[sim$D_B == 0])
SB_B      <- E_Y0_D1_B - E_Y0_D0_B
SDO_B     <- mean(sim$Y_B[sim$D_B == 1]) - mean(sim$Y_B[sim$D_B == 0])

cat("\n=== Scenario B: Randomization (D ~ Bernoulli(0.5)) ===\n")
cat(sprintf("Fraction treated:  %.1f%%\n", mean(sim$D_B)*100))
cat(sprintf("E[Y0 | D=1]:  %.1f lbs\n", E_Y0_D1_B))
cat(sprintf("E[Y0 | D=0]:  %.1f lbs\n", E_Y0_D0_B))
cat(sprintf("Selection bias:   %.2f lbs  (~0)\n", SB_B))
cat(sprintf("ATE:              %d  lbs\n", ATE))
cat(sprintf("SDO:              %.2f lbs  (~ATE!)\n", SDO_B))
cat(sprintf("ATE + SB:         %.2f lbs  (= SDO: decomposition checks out)\n", ATE + SB_B))
fit_B <- lm(Y_B ~ D_B, data = sim)
cat(sprintf("OLS coef on D_B:  %.2f lbs\n", coef(fit_B)["D_B"]))

# ============================================================================
# FIGURE 2: Love plot — covariate balance comparison
# ============================================================================

compute_smds <- function(d_vec, data) {
  covs <- list(
    "Age (years)"       = data$age,
    "% Female"          = data$female * 100,
    "% POC"             = data$poc * 100,
    "% College"         = data$college * 100,
    "Baseline weight"   = data$Y0
  )
  map_dfr(names(covs), function(cov_name) {
    x  <- covs[[cov_name]]
    t1 <- x[d_vec == 1]; t0 <- x[d_vec == 0]
    tibble(
      Covariate = cov_name,
      SMD       = (mean(t1) - mean(t0)) / sqrt((var(t1) + var(t0)) / 2)
    )
  })
}

smd_A <- compute_smds(sim$D_A, sim) |> mutate(Scenario = "A: Selection")
smd_B <- compute_smds(sim$D_B, sim) |> mutate(Scenario = "B: Randomization")

smd_all <- bind_rows(smd_A, smd_B) |>
  mutate(
    Scenario  = factor(Scenario, levels = c("A: Selection", "B: Randomization")),
    Covariate = factor(Covariate,
                       levels = rev(c("Age (years)", "% Female", "% POC",
                                      "% College", "Baseline weight")))
  )

# Print SMD table
cat("\n=== Standardized Mean Differences ===\n")
smd_all |> pivot_wider(names_from=Scenario, values_from=SMD) |>
  mutate(across(where(is.numeric), ~round(.x, 3))) |> print()

p_love <- ggplot(smd_all, aes(x = SMD, y = Covariate,
                               color = Scenario, shape = Scenario)) +
  geom_vline(xintercept = 0, color = "gray35", linewidth = 0.7) +
  geom_vline(xintercept = c(-0.1, 0.1), color = "gray70",
             linetype = "dotted", linewidth = 0.5) +
  geom_segment(data = smd_all |> pivot_wider(names_from = Scenario, values_from = SMD),
               aes(x = `A: Selection`, xend = `B: Randomization`,
                   y = Covariate, yend = Covariate),
               color = "gray80", linewidth = 0.6, inherit.aes = FALSE) +
  geom_point(size = 4.5, alpha = 0.95) +
  annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0.5, ymax = 5.5,
           fill = col_ate, alpha = 0.06) +
  annotate("text", x = 0, y = 5.6, label = "acceptable balance zone",
           color = col_ate, size = 2.9) +
  scale_color_manual(
    values = c("A: Selection" = col_sdo, "B: Randomization" = col_ate),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("A: Selection" = 16, "B: Randomization" = 17),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(-0.2, 1.8, 0.4)) +
  labs(x = "Standardized Mean Difference (Treated \u2212 Control)",
       y = NULL,
       title = "Love plot: selection creates large imbalances, randomization closes them") +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "top",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 11.5))

ggsave(file.path(out_path, "wt_love_plot.pdf"),
       p_love, width = 7.5, height = 3.5, device = cairo_pdf)
message("wt_love_plot.pdf written")

# ============================================================================
# FIGURE 3: Group means — what we observe under each scenario
# ============================================================================

grp_means <- tibble(
  Scenario = factor(
    c("A: Selection","A: Selection","B: Randomization","B: Randomization"),
    levels = c("A: Selection","B: Randomization")
  ),
  Group   = rep(c("Control (D=0)","Treated (D=1)"), 2),
  Mean_Y  = c(
    mean(sim$Y_A[sim$D_A == 0]),
    mean(sim$Y_A[sim$D_A == 1]),
    mean(sim$Y_B[sim$D_B == 0]),
    mean(sim$Y_B[sim$D_B == 1])
  )
)

sdo_ann <- tibble(
  Scenario = factor(c("A: Selection","B: Randomization"),
                    levels = c("A: Selection","B: Randomization")),
  label    = c(
    sprintf("SDO = +%.1f lbs\n(\u2260 ATE = \u221215)", SDO_A),
    sprintf("SDO = %.1f lbs\n(\u2248 ATE = \u221215)", SDO_B)
  ),
  x = c(1.5, 1.5),
  y = c(215, 195)
)

p_means <- ggplot(grp_means, aes(x = Group, y = Mean_Y, fill = Group)) +
  geom_col(width = 0.58, alpha = 0.88) +
  geom_text(aes(label = sprintf("%.1f lbs", Mean_Y)),
            vjust = -0.45, size = 3.4, fontface = "bold") +
  geom_text(data = sdo_ann, aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size = 3.2, color = col_sdo, fontface = "bold", lineheight = 1.1) +
  facet_wrap(~Scenario) +
  scale_fill_manual(values = c("Control (D=0)" = col_ctrl,
                                "Treated (D=1)" = col_trt)) +
  scale_y_continuous(labels = function(x) paste0(x," lbs")) +
  coord_cartesian(ylim = c(150, 230)) +
  labs(x = NULL, y = "Mean observed weight (lbs)", fill = NULL,
       title = "Selection makes the pill look harmful. Randomization reveals the truth.") +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        strip.text         = element_text(face = "bold", size = 12),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold", size = 11.5))

ggsave(file.path(out_path, "wt_means_compare.pdf"),
       p_means, width = 8.2, height = 3.8, device = cairo_pdf)
message("wt_means_compare.pdf written")

# ============================================================================
# SUMMARY TABLE
# ============================================================================
cat("\n=== Summary: ATE / SDO / Selection Bias ===\n")
cat(sprintf("                  Scenario A    Scenario B\n"))
cat(sprintf("True ATE:         %6d lbs    %6d lbs\n", ATE, ATE))
cat(sprintf("E[Y0 | D=1]:      %6.1f lbs    %6.1f lbs\n", E_Y0_D1_A, E_Y0_D1_B))
cat(sprintf("E[Y0 | D=0]:      %6.1f lbs    %6.1f lbs\n", E_Y0_D0_A, E_Y0_D0_B))
cat(sprintf("Selection bias:   %6.1f lbs    %6.2f lbs\n", SB_A, SB_B))
cat(sprintf("SDO:              %6.1f lbs    %6.2f lbs\n", SDO_A, SDO_B))
cat(sprintf("OLS coef:         %6.2f lbs    %6.2f lbs\n",
            coef(fit_A)["D_A"], coef(fit_B)["D_B"]))
cat(sprintf("SDO = ATE + SB:   %.1f = %d + %.1f = %.1f  [check]\n",
            SDO_A, ATE, SB_A, ATE + SB_A))
cat(sprintf("                  %.2f = %d + %.2f = %.2f  [check]\n",
            SDO_B, ATE, SB_B, ATE + SB_B))
message("\nAll figures written to: ", out_path)
