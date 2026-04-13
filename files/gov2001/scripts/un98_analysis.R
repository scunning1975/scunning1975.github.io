# ============================================================
# Gov 2001 Lecture 11b — Motivating Example
# UN98 Cross-National Infant Mortality Data (carData::UN98)
#
# Research question: What predicts infant mortality across nations?
#   infantMortality ~ log(GDPperCapita) + tfr + illiteracyFemale
#
# Key teaching goals:
#   1. OLS mechanics on real data
#   2. Visualize heteroskedasticity (fan shape at low GDP)
#   3. Compare naive vs robust (HC3) standard errors
#   4. Breusch-Pagan test
#   5. F-test and R-squared
# ============================================================

# Set working directory to script location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  script_path <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
  if (!is.null(script_path)) setwd(dirname(script_path))
}

# Required packages
library(tidyverse)
library(sandwich)
library(lmtest)

# Harvard color palette
crimson <- "#A51C30"
navy    <- "#2D3748"
ocean   <- "#2B6CB0"
slate   <- "#4A5568"
forest  <- "#276749"
warmgray <- "#718096"

# ============================================================
# 1. Load and prepare data
# ============================================================

data(UN98, package = "carData")

un <- UN98 |>
  select(infantMortality, GDPperCapita, tfr, illiteracyFemale) |>
  na.omit() |>
  mutate(log_gdp = log(GDPperCapita))

cat("N =", nrow(un), "countries\n")
cat("Infant mortality:  range =", paste(range(un$infantMortality), collapse = " to "), "\n")
cat("GDP per capita:    range = $", paste(round(range(un$GDPperCapita)), collapse = " to $"), "\n\n")

# ============================================================
# 2. Fit OLS model
# ============================================================

m <- lm(infantMortality ~ log_gdp + tfr + illiteracyFemale, data = un)

cat("=== OLS Estimates (Naive SEs) ===\n")
print(round(coef(summary(m)), 3))

cat("\nR² =", round(summary(m)$r.squared, 3),
    "  Adj R² =", round(summary(m)$adj.r.squared, 3),
    "  F =", round(summary(m)$fstatistic[1], 1), "\n\n")

# ============================================================
# 3. Robust (HC3) standard errors
# ============================================================

cat("=== HC3 Robust SEs ===\n")
print(round(coeftest(m, vcov = vcovHC(m, "HC3")), 3))

# SE comparison table
se_naive  <- coef(summary(m))[, "Std. Error"]
se_robust <- sqrt(diag(vcovHC(m, "HC3")))

cat("\n=== SE Comparison ===\n")
comp <- data.frame(
  Estimate   = round(coef(m), 3),
  SE_naive   = round(se_naive, 3),
  SE_robust  = round(se_robust, 3),
  pct_change = paste0(round(100 * (se_robust / se_naive - 1), 1), "%"),
  t_naive    = round(coef(m) / se_naive, 2),
  t_robust   = round(coef(m) / se_robust, 2)
)
print(comp)

# CI for log_gdp
b <- coef(m)["log_gdp"]
cat("\n95% CI for log_gdp:\n")
cat("  Naive:  [", round(b - 1.96 * se_naive["log_gdp"], 2),
    ",", round(b + 1.96 * se_naive["log_gdp"], 2), "]  width =",
    round(2 * 1.96 * se_naive["log_gdp"], 2), "\n")
cat("  Robust: [", round(b - 1.96 * se_robust["log_gdp"], 2),
    ",", round(b + 1.96 * se_robust["log_gdp"], 2), "]  width =",
    round(2 * 1.96 * se_robust["log_gdp"], 2), "\n\n")

# ============================================================
# 4. Breusch-Pagan test
# ============================================================

cat("=== Breusch-Pagan Test ===\n")
print(bptest(m))

# ============================================================
# 5. F-test
# ============================================================

cat("\n=== F-test (restricted vs full) ===\n")
m0 <- lm(infantMortality ~ 1, data = un)
print(anova(m0, m))

# ============================================================
# 6. Figure 1: Scatter + OLS line (infantMortality ~ log_gdp)
# ============================================================

png("../fig_un98_scatter.png", width = 7, height = 3.8, units = "in", res = 200)

un_plot <- un |>
  mutate(fitted = fitted(m), resid = residuals(m))

ggplot(un_plot, aes(x = log_gdp, y = infantMortality)) +
  geom_point(color = crimson, alpha = 0.65, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, color = navy, linewidth = 1.0) +
  labs(
    x = "Log GDP per capita",
    y = "Infant mortality (per 1,000 live births)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title  = element_text(color = slate),
    axis.text   = element_text(color = slate)
  )

dev.off()
cat("Saved fig_un98_scatter.png\n")

# ============================================================
# 7. Figure 2: Residuals vs Fitted (fan shape)
# ============================================================

png("../fig_un98_resid.png", width = 7, height = 3.8, units = "in", res = 200)

ggplot(un_plot, aes(x = fitted, y = resid)) +
  geom_point(color = crimson, alpha = 0.65, size = 1.8) +
  geom_hline(yintercept = 0, color = navy, linewidth = 0.8, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = ocean, linewidth = 0.9) +
  labs(
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title  = element_text(color = slate),
    axis.text   = element_text(color = slate)
  )

dev.off()
cat("Saved fig_un98_resid.png\n")
