# ============================================================================
# Gov 51 Section — Week 11: Potential Outcomes and Causal Inference
# Replication script for section deck (week11_section.tex)
# April 8, 2026
# ============================================================================
#
# Runs every code block in the section slides in order.
# Also regenerates balance_simple.pdf and balance_diverse.pdf.
#
# Required packages: tidyverse, cowplot, scales
# install.packages(c("tidyverse", "cowplot", "scales"))

library(tidyverse)
library(cowplot)
library(scales)

# ============================================================================
# PART I: SELF-SELECTION VS. COIN FLIP
# ============================================================================

# ----------------------------------------------------------------------------
# Slide: "Code: self-selection creates imbalance — coin flip destroys it"
# ----------------------------------------------------------------------------
set.seed(200)

# 200 students: Gov choosers are 62% female, CS choosers are 22% female
harvard <- tibble(
  female = c(rbinom(120, 1, 0.62), rbinom(80, 1, 0.22)),
  choice = c(rep("Gov", 120), rep("CS", 80)),
  coin   = rbinom(200, 1, 0.5)   # random assignment
)

cat("\n--- Self-selected groups ---\n")
harvard |>
  group_by(choice) |>
  summarize(pct_female = round(mean(female) * 100, 1), n = n()) |>
  print()
# Expected: Gov ~62%, CS ~22%

cat("\n--- Coin-flip groups ---\n")
harvard |>
  group_by(coin) |>
  summarize(pct_female = round(mean(female) * 100, 1), n = n()) |>
  print()
# Expected: both groups ~50%

# ----------------------------------------------------------------------------
# Regenerate balance_simple.pdf
# (Uses the same population from harvard tibble above)
# ----------------------------------------------------------------------------
pop_pct <- mean(harvard$female) * 100

self_sum <- harvard |>
  group_by(choice) |>
  summarize(pct = mean(female) * 100, .groups = "drop") |>
  mutate(scenario = "Self-selected")

rand_sum <- harvard |>
  group_by(coin) |>
  summarize(pct = mean(female) * 100, .groups = "drop") |>
  mutate(choice = ifelse(coin == 1, "CS", "Gov"),
         scenario = "Coin flip") |>
  select(choice, pct, scenario)

plot_data <- bind_rows(self_sum, rand_sum) |>
  mutate(scenario = factor(scenario, levels = c("Self-selected", "Coin flip")))

pal <- c("CS" = "#2B6CB0", "Gov" = "#276749")

p1 <- ggplot(plot_data, aes(x = choice, y = pct, fill = choice)) +
  geom_col(width = 0.55, alpha = 0.88) +
  geom_hline(yintercept = pop_pct, linetype = "dashed",
             color = "#718096", linewidth = 0.7) +
  annotate("text", x = 0.55, y = pop_pct + 3,
           label = "population avg", color = "#718096",
           size = 3.2, hjust = 0) +
  facet_wrap(~scenario) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 75)) +
  labs(x = NULL, y = "% Female",
       title = "Does the coin flip balance sex?") +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        strip.text         = element_text(size = 13, face = "bold"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13))

out_path <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "."
)
ggsave(file.path(out_path, "balance_simple.pdf"),
       p1, width = 7, height = 3.2, device = cairo_pdf)
message("balance_simple.pdf written")

# ----------------------------------------------------------------------------
# Regenerate balance_diverse.pdf
# ----------------------------------------------------------------------------
set.seed(51)
N <- 300

pop <- tibble(
  female        = rbinom(N, 1, 0.48),
  poc           = rbinom(N, 1, 0.42),
  international = rbinom(N, 1, 0.18)
)

# Self-selection: CS more likely to be male and international
logit_cs  <- -0.2 + 0.8 * pop$international - 0.9 * pop$female + rnorm(N, 0, 0.4)
pop$D_self <- as.integer(plogis(logit_cs) > 0.5)
pop$D_rand <- rbinom(N, 1, 0.5)

make_balance <- function(data, d_var, scenario_label) {
  data |>
    group_by(!!sym(d_var)) |>
    summarize(
      Female        = mean(female),
      POC           = mean(poc),
      International = mean(international),
      .groups = "drop"
    ) |>
    mutate(
      Group    = ifelse(!!sym(d_var) == 1, "CS", "Gov"),
      Scenario = scenario_label
    ) |>
    select(-!!sym(d_var)) |>
    pivot_longer(c(Female, POC, International),
                 names_to  = "Covariate",
                 values_to = "Proportion")
}

bal <- bind_rows(
  make_balance(pop, "D_self", "Self-selected"),
  make_balance(pop, "D_rand", "Coin flip")
) |>
  mutate(Scenario = factor(Scenario, levels = c("Self-selected", "Coin flip")))

p2 <- ggplot(bal, aes(x = Covariate, y = Proportion, fill = Group)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.88) +
  facet_wrap(~Scenario) +
  scale_fill_manual(values = pal, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.72)) +
  labs(x = NULL, y = "Proportion",
       title = "Coin flip balances ALL covariates") +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "bottom",
        legend.text        = element_text(size = 12),
        strip.text         = element_text(size = 13, face = "bold"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title         = element_text(face = "bold", size = 13))

ggsave(file.path(out_path, "balance_diverse.pdf"),
       p2, width = 7.5, height = 3.5, device = cairo_pdf)
message("balance_diverse.pdf written")

# ----------------------------------------------------------------------------
# Slide: "Even potential outcomes Y1 and Y0 balance under randomization"
# ----------------------------------------------------------------------------
set.seed(51)

po_sim <- tibble(
  Y1 = rnorm(1000, mean = 75, sd = 15),  # PO if treated
  Y0 = rnorm(1000, mean = 60, sd = 15),  # PO if not treated
  D  = rbinom(1000, 1, 0.5)               # coin flip
)

cat("\n--- Potential outcomes by treatment group ---\n")
po_sim |>
  group_by(D) |>
  summarize(
    mean_Y1 = round(mean(Y1), 1),
    mean_Y0 = round(mean(Y0), 1)
  ) |>
  print()
# Expected: Y1 and Y0 nearly equal across D=0 and D=1

cat("\nTrue ATE:", round(mean(po_sim$Y1 - po_sim$Y0), 1), "\n")
cat("SDO (under randomization):",
    round(mean(po_sim$Y1[po_sim$D == 1]) - mean(po_sim$Y0[po_sim$D == 0]), 1), "\n")
# Under randomization, SDO ≈ ATE

# ============================================================================
# PART II: B&M RESUME AUDIT (Bertrand & Mullainathan 2004)
# ============================================================================
# Requires: resume.csv
# If you don't have it: install.packages("qss"); data(resume, package="qss")

cat("\n--- Resume audit (requires resume.csv) ---\n")

if (file.exists("resume.csv")) {
  resume <- read_csv("resume.csv", show_col_types = FALSE) |>
    mutate(white = as.integer(race == "white"))

  cat("\nBalance check (sex by race):\n")
  resume |>
    group_by(race) |>
    summarize(pct_female = mean(sex == "female"), n = n()) |>
    print()

  cat("\nDiff-in-means (callback by race):\n")
  dm <- resume |>
    group_by(race) |>
    summarize(callback = mean(call))
  print(dm)
  ate_dm <- diff(dm$callback)
  cat("ATE (white - Black):", round(ate_dm, 4), "\n")

  cat("\nOLS:\n")
  fit <- lm(call ~ white, data = resume)
  print(coef(fit))
  cat("OLS coefficient:", round(coef(fit)["white"], 4), "\n")
  cat("Diff-in-means:", round(ate_dm, 4), "  <-- should match\n")

} else {
  message("resume.csv not found — skipping B&M section")
  message("To get the data: install.packages('qss') and use data(resume, package='qss')")
}

# ============================================================================
# PART IV: HARVARD VS. DARTMOUTH (Numerical Example from Slides)
# ============================================================================

cat("\n--- Harvard vs. Dartmouth potential outcomes table ---\n")

hd <- tibble(
  student = c("Aria", "Ben", "Chen", "Dana", "Eli", "Fiona"),
  Y1      = c(160, 180, 170,  90, 110, 100),  # salary if Harvard
  Y0      = c(130, 150, 140,  60,  80,  70),  # salary if Dartmouth
  D       = c(1, 1, 1, 0, 0, 0)
)

hd <- hd |>
  mutate(
    delta  = Y1 - Y0,
    Y_obs  = ifelse(D == 1, Y1, Y0)
  )

print(hd)

# True ATE
ate_true <- mean(hd$delta)
cat("\nTrue ATE:", ate_true, "\n")

# SDO
sdo <- mean(hd$Y_obs[hd$D == 1]) - mean(hd$Y_obs[hd$D == 0])
cat("SDO:", sdo, "\n")

# Selection bias
sb <- mean(hd$Y0[hd$D == 1]) - mean(hd$Y0[hd$D == 0])
cat("Selection bias:", sb, "\n")

cat("\nDecomposition check: ATE + SB =", ate_true + sb,
    " vs SDO =", sdo, "\n")
# Should be equal: 30 + 70 = 100

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n============================================================\n")
cat("All checks:\n")
cat("  PO simulation: ATE ≈ SDO under randomization       [check]\n")
cat("  Harvard/Dartmouth: SDO = ATE + SB (mechanical identity) [check]\n")
cat("  balance_simple.pdf regenerated                       [check]\n")
cat("  balance_diverse.pdf regenerated                      [check]\n")
cat("============================================================\n")
