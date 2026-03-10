# week06b_wages.R
# Gov 51 — Applied Regression Interpretation with Gender Wage Gap
# Uses wooldridge::wage1 dataset
# Run this FIRST to get exact coefficients for the lecture slides

library(wooldridge)
library(tidyverse)

# Load the data
data(wage1)

cat("=== Dataset Overview ===\n")
cat("Observations:", nrow(wage1), "\n")
cat("Key variables: wage, educ, exper, tenure, female, married, nonwhite\n\n")

# Summary stats
cat("=== Summary Statistics ===\n")
wage1 %>%
  summarize(
    n = n(),
    mean_wage = mean(wage),
    sd_wage = sd(wage),
    mean_educ = mean(educ),
    mean_exper = mean(exper),
    mean_tenure = mean(tenure),
    pct_female = mean(female) * 100,
    pct_married = mean(married) * 100
  ) %>%
  print()

cat("\n=== Means by Gender ===\n")
wage1 %>%
  group_by(female) %>%
  summarize(
    n = n(),
    mean_wage = mean(wage),
    sd_wage = sd(wage),
    mean_educ = mean(educ),
    mean_exper = mean(exper),
    mean_tenure = mean(tenure),
    pct_married = mean(married) * 100
  ) %>%
  print()

# -------------------------------------------------------
# Model 1: wage ~ female (bivariate, the raw gap)
# -------------------------------------------------------
cat("\n=== MODEL 1: wage ~ female ===\n")
m1 <- lm(wage ~ female, data = wage1)
summary(m1)

cat("\nModel 1 coefficients:\n")
cat("  Intercept (male mean):", round(coef(m1)[1], 4), "\n")
cat("  female (raw gap):", round(coef(m1)[2], 4), "\n")
cat("  R-squared:", round(summary(m1)$r.squared, 4), "\n")
cat("  SE of female:", round(summary(m1)$coefficients[2, 2], 4), "\n")
cat("  t-stat of female:", round(summary(m1)$coefficients[2, 3], 4), "\n")
cat("  Female mean wage:", round(coef(m1)[1] + coef(m1)[2], 4), "\n")

# -------------------------------------------------------
# Model 2: wage ~ female + educ + exper + tenure
# -------------------------------------------------------
cat("\n=== MODEL 2: wage ~ female + educ + exper + tenure ===\n")
m2 <- lm(wage ~ female + educ + exper + tenure, data = wage1)
summary(m2)

cat("\nModel 2 coefficients:\n")
cat("  Intercept:", round(coef(m2)[1], 4), "\n")
cat("  female:", round(coef(m2)[2], 4), "\n")
cat("  educ:", round(coef(m2)[3], 4), "\n")
cat("  exper:", round(coef(m2)[4], 4), "\n")
cat("  tenure:", round(coef(m2)[5], 4), "\n")
cat("  R-squared:", round(summary(m2)$r.squared, 4), "\n")
cat("  SE of female:", round(summary(m2)$coefficients[2, 2], 4), "\n")

# Plug-in prediction: woman, educ=16, exper=10, tenure=5
pred_woman_m2 <- coef(m2)[1] + coef(m2)[2]*1 + coef(m2)[3]*16 + coef(m2)[4]*10 + coef(m2)[5]*5
pred_man_m2 <- coef(m2)[1] + coef(m2)[2]*0 + coef(m2)[3]*16 + coef(m2)[4]*10 + coef(m2)[5]*5
cat("\nPlug-in predictions (educ=16, exper=10, tenure=5):\n")
cat("  Woman:", round(pred_woman_m2, 4), "\n")
cat("  Man:", round(pred_man_m2, 4), "\n")
cat("  Gap:", round(pred_woman_m2 - pred_man_m2, 4), "\n")

# -------------------------------------------------------
# Model 3: wage ~ female + married + female:married + educ + exper + tenure
# -------------------------------------------------------
cat("\n=== MODEL 3: wage ~ female * married + educ + exper + tenure ===\n")
m3 <- lm(wage ~ female * married + educ + exper + tenure, data = wage1)
summary(m3)

cat("\nModel 3 coefficients:\n")
cat("  Intercept:", round(coef(m3)[1], 4), "\n")
cat("  female:", round(coef(m3)[2], 4), "\n")
cat("  married:", round(coef(m3)[3], 4), "\n")
cat("  educ:", round(coef(m3)[4], 4), "\n")
cat("  exper:", round(coef(m3)[5], 4), "\n")
cat("  tenure:", round(coef(m3)[6], 4), "\n")
cat("  female:married:", round(coef(m3)[7], 4), "\n")
cat("  R-squared:", round(summary(m3)$r.squared, 4), "\n")

# 2x2 predicted means (educ=12, exper=10, tenure=5)
educ_val <- 12
exper_val <- 10
tenure_val <- 5

single_man <- coef(m3)[1] + coef(m3)[4]*educ_val + coef(m3)[5]*exper_val + coef(m3)[6]*tenure_val
married_man <- single_man + coef(m3)[3]
single_woman <- single_man + coef(m3)[2]
married_woman <- single_man + coef(m3)[2] + coef(m3)[3] + coef(m3)[7]

cat("\n2x2 Predicted Means (educ=12, exper=10, tenure=5):\n")
cat("  Single man:", round(single_man, 4), "\n")
cat("  Married man:", round(married_man, 4), "\n")
cat("  Single woman:", round(single_woman, 4), "\n")
cat("  Married woman:", round(married_woman, 4), "\n")

cat("\nMarriage premium for men:", round(married_man - single_man, 4), "\n")
cat("Marriage premium for women:", round(married_woman - single_woman, 4), "\n")

# -------------------------------------------------------
# Figures
# -------------------------------------------------------

# Color palette (matches gov51-style.sty)
crimson <- "#A51C30"
navy <- "#1E3C72"
blue <- "#2980B9"
teal <- "#16A085"
orange <- "#E67E22"
gray_col <- "#7F8C8D"

theme_gov51 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = navy, face = "bold", size = 16),
    plot.subtitle = element_text(color = gray_col, size = 12),
    axis.title = element_text(color = navy),
    panel.grid.minor = element_blank()
  )

# Figure 1: Histogram of wages by gender
p1 <- ggplot(wage1, aes(x = wage, fill = factor(female, labels = c("Men", "Women")))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", color = "white") +
  scale_fill_manual(values = c(navy, crimson), name = "") +
  geom_vline(data = wage1 %>% group_by(female) %>% summarize(m = mean(wage)),
             aes(xintercept = m, color = factor(female, labels = c("Men", "Women"))),
             linewidth = 1.2, linetype = "dashed", show.legend = FALSE) +
  scale_color_manual(values = c(navy, crimson)) +
  labs(x = "Hourly Wage ($)", y = "Count",
       title = "Distribution of Wages by Gender") +
  theme_gov51 +
  theme(legend.position = c(0.8, 0.8))

ggsave("figures/wage_histogram_gender.pdf", p1, width = 8, height = 5)

# Figure 2: 2x2 bar chart of predicted means
pred_df <- data.frame(
  Gender = rep(c("Men", "Women"), each = 2),
  Marital = rep(c("Single", "Married"), 2),
  Predicted = c(single_man, married_man, single_woman, married_woman)
)
pred_df$Gender <- factor(pred_df$Gender, levels = c("Men", "Women"))
pred_df$Marital <- factor(pred_df$Marital, levels = c("Single", "Married"))

p2 <- ggplot(pred_df, aes(x = Gender, y = Predicted, fill = Marital)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(Predicted, 2))),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4.5,
            fontface = "bold", color = navy) +
  scale_fill_manual(values = c(blue, teal), name = "") +
  labs(x = "", y = "Predicted Wage ($/hr)",
       title = "Marriage Premium: Men vs. Women") +
  ylim(0, max(pred_df$Predicted) * 1.15) +
  theme_gov51 +
  theme(legend.position = "top")

ggsave("figures/wage_2x2_marriage.pdf", p2, width = 7, height = 5)

cat("\n=== Figures saved to figures/ ===\n")
cat("  wage_histogram_gender.pdf\n")
cat("  wage_2x2_marriage.pdf\n")
cat("\nDone!\n")
