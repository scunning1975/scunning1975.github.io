# ============================================================================
# Gov 51 Lecture — Week 11: Broockman & Kalla (2016) Replication
# "Durably reducing transphobia: A field experiment on door-to-door canvassing"
# Science, 352(6282), 220-224.
# ============================================================================
#
# This script replicates every code slide in week11-tuesday-potential-outcomes.pdf.
# Run from the directory containing bk2016.csv.
#
# Required packages: tidyverse, estimatr
# install.packages(c("tidyverse", "estimatr"))

library(tidyverse)

# ============================================================================
# STEP 1: Load the data and check covariate balance
# ============================================================================

bk <- read_csv("bk2016.csv")

cat("=== Dataset overview ===\n")
cat("Rows:", nrow(bk), "| Cols:", ncol(bk), "\n")
cat("Column names:", paste(names(bk), collapse = ", "), "\n\n")

# Outcome: trans_therm_t3
#   Feeling thermometer toward transgender people (0 = very cold, 100 = very warm)
#   Measured 3 months after the canvassing experiment (t3 = third wave)
#
# Treatment: treatment (1 = received a 10-min door-to-door conversation)
#            treatment (0 = placebo canvassing on recycling)

# --- Balance check: is the randomization balanced? ---
cat("=== Step 1: Covariate balance (full sample, n = 1,825) ===\n")

bk |>
  group_by(treatment) |>
  summarize(
    pct_female   = round(mean(female),            3),
    mean_age     = round(mean(age, na.rm = TRUE), 1),
    pct_democrat = round(mean(democrat),          3),
    pct_white    = round(mean(white),             3),
    mean_therm0  = round(mean(trans_therm_t0, na.rm = TRUE), 2),
    n            = n()
  ) |>
  print()

# Expected: all covariates nearly identical across treatment=0 and treatment=1
# (Randomization was successful)


# ============================================================================
# STEP 2: Estimate the ATE as a difference in means (3-month follow-up)
# ============================================================================

cat("\n=== Step 2: Difference in means at t3 (3-month follow-up) ===\n")

# Restrict to respondents who completed the 3-month follow-up survey
bk_t3 <- bk |> filter(responded_t3 == 1)
cat("t3 respondents:", nrow(bk_t3), "\n")

bk_t3 |>
  group_by(treatment) |>
  summarize(
    mean_therm = round(mean(trans_therm_t3), 2),
    n          = n()
  ) |>
  print()

# ATE estimate: difference in group means
ate_hat <- mean(bk_t3$trans_therm_t3[bk_t3$treatment == 1]) -
           mean(bk_t3$trans_therm_t3[bk_t3$treatment == 0])

cat("\nATE estimate (SDO):", round(ate_hat, 2), "points on 0-100 scale\n")
# Expected: ~6.23


# ============================================================================
# STEP 3: OLS with binary treatment — same as difference in means
# ============================================================================

cat("\n=== Step 3: OLS regression (treatment as binary regressor) ===\n")

fit <- lm(trans_therm_t3 ~ treatment, data = bk_t3)
print(round(coef(fit), 3))

cat("\n  Intercept = E[Y | treatment=0] =", round(coef(fit)[1], 2), "\n")
cat("  Coefficient = difference in means =", round(coef(fit)[2], 2), "(same as SDO)\n")


# ============================================================================
# STEP 4: Neyman standard error
# ============================================================================

cat("\n=== Step 4: Neyman (randomization-based) standard error ===\n")

s1 <- var(bk_t3$trans_therm_t3[bk_t3$treatment == 1])
s0 <- var(bk_t3$trans_therm_t3[bk_t3$treatment == 0])
n1 <- sum(bk_t3$treatment == 1)
n0 <- sum(bk_t3$treatment == 0)

se_neyman <- sqrt(s1 / n1 + s0 / n0)
t_stat    <- ate_hat / se_neyman
p_val     <- 2 * pnorm(-abs(t_stat))

cat("s1^2 (treated variance):", round(s1, 2), "| n1:", n1, "\n")
cat("s0^2 (control variance):", round(s0, 2), "| n0:", n0, "\n")
cat("Neyman SE:", round(se_neyman, 2), "\n")
cat("t-statistic:", round(t_stat, 2), "\n")
cat("p-value (two-tailed):", round(p_val, 4), "\n")
cat("95% CI: [", round(ate_hat - 1.96 * se_neyman, 2), ",",
    round(ate_hat + 1.96 * se_neyman, 2), "]\n")


# ============================================================================
# STEP 5: OLS with HC2 SE = Neyman SE exactly
# ============================================================================

cat("\n=== Step 5: OLS HC2 SE == Neyman SE ===\n")

if (requireNamespace("estimatr", quietly = TRUE)) {
  library(estimatr)
  fit_hc2 <- lm_robust(trans_therm_t3 ~ treatment, data = bk_t3, se_type = "HC2")
  cat("HC2 SE from lm_robust:", round(summary(fit_hc2)$coefficients["treatment", "Std. Error"], 2), "\n")
  cat("Neyman SE (manual):   ", round(se_neyman, 2), "\n")
  cat("(These should match exactly)\n")
} else {
  message("Install 'estimatr' to verify HC2 == Neyman SE: install.packages('estimatr')")
  cat("Manual Neyman SE:", round(se_neyman, 2), "\n")
  cat("OLS classical SE:", round(summary(fit)$coefficients["treatment", "Std. Error"], 2),
      " (slightly different — assumes equal variances)\n")
}


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n============================================================\n")
cat("Broockman & Kalla (2016) — Key Results\n")
cat("------------------------------------------------------------\n")
cat("  Outcome: transgender feeling thermometer (0–100)\n")
cat("  Timepoint: 3 months post-canvassing\n")
cat("  Control mean:  ", round(mean(bk_t3$trans_therm_t3[bk_t3$treatment==0]), 2), "\n")
cat("  Treated mean:  ", round(mean(bk_t3$trans_therm_t3[bk_t3$treatment==1]), 2), "\n")
cat("  ATE (SDO):     ", round(ate_hat, 2), "points\n")
cat("  Neyman SE:     ", round(se_neyman, 2), "\n")
cat("  t-stat:        ", round(t_stat, 2), "\n")
cat("  p-value:       ", round(p_val, 4), "\n")
cat("============================================================\n")
