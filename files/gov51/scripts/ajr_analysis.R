# ============================================================================
# Gov 51 Lecture — Week 11: AJR (2001) IV Analysis
# Acemoglu, Johnson & Robinson (2001), "The Colonial Origins of Comparative
# Development: An Empirical Investigation", AER 91(5), 1369–1401.
# ============================================================================
#
# Variables:
#   logem4   = log settler mortality (instrument Z)
#   avexpr   = avg protection against expropriation, 0–10 (treatment D)
#   logpgp95 = log GDP per capita, 1995 (outcome Y)
#
# Required packages: tidyverse, estimatr
# install.packages(c("tidyverse", "estimatr"))

library(tidyverse)
library(estimatr)

# ============================================================================
# Set working directory to script location (works in RStudio and Rscript)
# ============================================================================

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  script_path <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
  if (!is.null(script_path)) setwd(dirname(script_path))
}

# ============================================================================
# 0. Harvard color palette (matches gov51-style.sty)
# ============================================================================

crimson  <- "#A51C30"   # harvardcrimson
navy     <- "#1E3C72"   # harvarddark
blue     <- "#2980B9"   # harvardblue
teal     <- "#1A6B5A"   # harvardteal
orange   <- "#D35400"   # harvardorange
warmgray <- "#BDC3C7"

# ============================================================================
# 1. Load data
# ============================================================================

ajr <- read_csv("ajr2001.csv")

cat("=== AJR 2001 Dataset ===\n")
cat("Observations:", nrow(ajr), "\n")
cat("Variables:", paste(names(ajr), collapse = ", "), "\n\n")

# Drop rows with missing values on key variables
ajr_clean <- ajr %>%
  filter(!is.na(logem4), !is.na(avexpr), !is.na(logpgp95))

cat("Complete cases:", nrow(ajr_clean), "\n\n")

# ============================================================================
# 2. OLS (biased baseline)
# ============================================================================

ols <- lm_robust(logpgp95 ~ avexpr, data = ajr_clean)

cat("=== OLS: log GDP ~ institutions ===\n")
cat(sprintf("  beta_OLS = %.3f  (SE = %.3f,  t = %.2f)\n\n",
            coef(ols)["avexpr"],
            ols$std.error["avexpr"],
            ols$statistic["avexpr"]))

# ============================================================================
# 3. First stage: avexpr ~ logem4
# ============================================================================

fs <- lm_robust(avexpr ~ logem4, data = ajr_clean)

cat("=== First Stage: institutions ~ log settler mortality ===\n")
cat(sprintf("  alpha_1  = %.3f  (SE = %.3f,  t = %.2f)\n",
            coef(fs)["logem4"],
            fs$std.error["logem4"],
            fs$statistic["logem4"]))
cat(sprintf("  F-stat   = %.1f\n\n", fs$fstatistic[1]))

# ============================================================================
# 4. Reduced form: logpgp95 ~ logem4
# ============================================================================

rf <- lm_robust(logpgp95 ~ logem4, data = ajr_clean)

cat("=== Reduced Form: log GDP ~ log settler mortality ===\n")
cat(sprintf("  pi_1     = %.3f  (SE = %.3f,  t = %.2f)\n\n",
            coef(rf)["logem4"],
            rf$std.error["logem4"],
            rf$statistic["logem4"]))

# ============================================================================
# 5. Wald estimator (manual)
# ============================================================================

wald <- coef(rf)["logem4"] / coef(fs)["logem4"]
cat(sprintf("=== Wald estimator: RF / FS = %.3f / %.3f = %.3f ===\n\n",
            coef(rf)["logem4"], coef(fs)["logem4"], wald))

# ============================================================================
# 6. 2SLS via iv_robust
# ============================================================================

iv <- iv_robust(logpgp95 ~ avexpr | logem4, data = ajr_clean)

cat("=== 2SLS: log GDP ~ institutions | log settler mortality ===\n")
cat(sprintf("  beta_2SLS = %.3f  (SE = %.3f,  t = %.2f,  95%% CI [%.3f, %.3f])\n\n",
            coef(iv)["avexpr"],
            iv$std.error["avexpr"],
            iv$statistic["avexpr"],
            iv$conf.low["avexpr"],
            iv$conf.high["avexpr"]))

# ============================================================================
# 7. Figure 1: First Stage
# ============================================================================

# Fitted values for regression line
fs_line <- tibble(
  logem4 = seq(min(ajr_clean$logem4), max(ajr_clean$logem4), length.out = 200),
  avexpr = coef(fs)["(Intercept)"] + coef(fs)["logem4"] * logem4
)

p_fs <- ggplot(ajr_clean, aes(x = logem4, y = avexpr)) +
  geom_point(color = crimson, size = 2.5, alpha = 0.75) +
  geom_line(data = fs_line, color = navy, linewidth = 1.0) +
  annotate("text", x = 7.2, y = 8.8,
           label = sprintf("Slope = %.3f\nF = %.1f",
                           coef(fs)["logem4"], fs$fstatistic[1]),
           color = navy, size = 3.2, hjust = 0) +
  labs(
    x = "Log Settler Mortality",
    y = "Institutional Quality (avexpr, 0–10)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(color = navy, face = "bold", size = 12),
    axis.title   = element_text(color = navy, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE")
  )

ggsave("fig_first_stage.png", p_fs, width = 7, height = 3.0, dpi = 200)
cat("Saved: fig_first_stage.png\n")

# ============================================================================
# 8. Figure 2: Reduced Form
# ============================================================================

rf_line <- tibble(
  logem4  = seq(min(ajr_clean$logem4), max(ajr_clean$logem4), length.out = 200),
  logpgp95 = coef(rf)["(Intercept)"] + coef(rf)["logem4"] * logem4
)

p_rf <- ggplot(ajr_clean, aes(x = logem4, y = logpgp95)) +
  geom_point(color = blue, size = 2.5, alpha = 0.75) +
  geom_line(data = rf_line, color = navy, linewidth = 1.0) +
  annotate("text", x = 7.2, y = 9.5,
           label = sprintf("Slope = %.3f", coef(rf)["logem4"]),
           color = navy, size = 3.2, hjust = 0) +
  labs(
    x = "Log Settler Mortality",
    y = "Log GDP per Capita, 1995"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(color = navy, face = "bold", size = 12),
    axis.title   = element_text(color = navy, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE")
  )

ggsave("fig_reduced_form.png", p_rf, width = 7, height = 3.0, dpi = 200)
cat("Saved: fig_reduced_form.png\n")

# ============================================================================
# 9. Figure 3: 2SLS projection — RF onto FS
#    x-axis: D-hat (fitted institutions from Stage 1)
#    y-axis: log GDP per capita
#    slope  = 2SLS coefficient (= RF/FS = 0.917)
# ============================================================================

ajr_clean <- ajr_clean %>%
  mutate(avexpr_hat = fitted(lm(avexpr ~ logem4, data = ajr_clean)))

iv_line <- tibble(
  avexpr_hat = seq(min(ajr_clean$avexpr_hat), max(ajr_clean$avexpr_hat), length.out = 200),
  logpgp95   = coef(iv)["(Intercept)"] + coef(iv)["avexpr"] * avexpr_hat
)

p_2sls <- ggplot(ajr_clean, aes(x = avexpr_hat, y = logpgp95)) +
  geom_point(color = teal, size = 2.5, alpha = 0.75) +
  geom_line(data = iv_line, color = navy, linewidth = 1.0) +
  annotate("text", x = max(ajr_clean$avexpr_hat) - 0.3, y = min(ajr_clean$logpgp95) + 0.3,
           label = sprintf("Slope = %.3f  (= 2SLS)", coef(iv)["avexpr"]),
           color = navy, size = 3.2, hjust = 1) +
  labs(
    x = expression(hat(D)[i] ~ "(fitted institutions from Stage 1)"),
    y = "Log GDP per Capita, 1995"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(color = navy, face = "bold", size = 12),
    axis.title   = element_text(color = navy, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE")
  )

ggsave("fig_2sls_projection.png", p_2sls, width = 7, height = 3.0, dpi = 200)
cat("Saved: fig_2sls_projection.png\n")

# ============================================================================
# 10. Summary table of key numbers for slides
# ============================================================================

cat("\n=== NUMBERS FOR SLIDES ===\n")
cat(sprintf("OLS beta:     %.3f\n", coef(ols)["avexpr"]))
cat(sprintf("FS alpha_1:   %.3f  (SE %.3f, F = %.1f)\n",
            coef(fs)["logem4"], fs$std.error["logem4"], fs$fstatistic[1]))
cat(sprintf("RF pi_1:      %.3f  (SE %.3f)\n",
            coef(rf)["logem4"], rf$std.error["logem4"]))
cat(sprintf("Wald:         %.3f\n", wald))
cat(sprintf("2SLS beta:    %.3f  (SE %.3f)\n",
            coef(iv)["avexpr"], iv$std.error["avexpr"]))
