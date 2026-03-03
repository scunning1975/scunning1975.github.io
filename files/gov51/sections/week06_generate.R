# week06_generate.R
# Generates civic_knowledge.csv and all figures for Week 6 Section
# Gov 51 - March 4, 2026
# Run this BEFORE building the .tex file -- all slide numbers come from here

library(tidyverse)

set.seed(51)
n <- 2500

# ============================================================================
# GENERATE DATA
# ============================================================================

age        <- round(runif(n, 18, 85))
female     <- rbinom(n, 1, 0.52)
college    <- rbinom(n, 1, 0.35)
news_hours <- round(pmin(rgamma(n, shape = 2, rate = 1), 7), 1)
income     <- round(pmin(pmax(rlnorm(n, meanlog = log(50) + 0.3 * college, sdlog = 0.5), 20), 150), 1)

# DGP: knowledge = 25 + 0.4*age - 0.005*age^2 + 4*news_hours + 3*college + 8*college*female + 0.15*income + noise
knowledge_raw <- 25 + 0.4 * age - 0.005 * age^2 + 4 * news_hours +
                 3 * college + 8 * college * female + 0.15 * income +
                 rnorm(n, 0, 8)

# Clamp to 0-100
knowledge <- round(pmin(pmax(knowledge_raw, 0), 100), 1)

d <- tibble(knowledge, age, news_hours, female, college, income)

write_csv(d, "civic_knowledge.csv")
cat("Dataset saved: civic_knowledge.csv\n")
cat(sprintf("n = %d, mean knowledge = %.2f, sd knowledge = %.2f\n",
            nrow(d), mean(d$knowledge), sd(d$knowledge)))

# ============================================================================
# MODEL 1: BIVARIATE (news_hours)
# ============================================================================

fit1 <- lm(knowledge ~ news_hours, data = d)
cat("\n===== MODEL 1: knowledge ~ news_hours =====\n")
print(summary(fit1))

# ============================================================================
# MODEL 2: MULTIVARIATE (news_hours + age + female + college)
# ============================================================================

fit2 <- lm(knowledge ~ news_hours + age + female + college, data = d)
cat("\n===== MODEL 2: knowledge ~ news_hours + age + female + college =====\n")
print(summary(fit2))

# ============================================================================
# MODEL 3: POLYNOMIAL (age + age^2)
# ============================================================================

fit3 <- lm(knowledge ~ age + I(age^2), data = d)
cat("\n===== MODEL 3: knowledge ~ age + I(age^2) =====\n")
print(summary(fit3))

# Peak age
b1_age <- coef(fit3)["age"]
b2_age <- coef(fit3)["I(age^2)"]
peak_age <- -b1_age / (2 * b2_age)
cat(sprintf("\nPeak age = %.1f\n", peak_age))

# Predicted at 25, 50, 75
for (a in c(25, 50, 75)) {
  yhat <- predict(fit3, newdata = data.frame(age = a))
  cat(sprintf("  Predicted at age %d: %.1f\n", a, yhat))
}

# ============================================================================
# MODEL 4: INTERACTION (college * female)
# ============================================================================

fit4 <- lm(knowledge ~ college * female, data = d)
cat("\n===== MODEL 4: knowledge ~ college * female =====\n")
print(summary(fit4))

# Four predicted means
cat("\nFour group means from interaction model:\n")
for (c_val in c(0, 1)) {
  for (f_val in c(0, 1)) {
    yhat <- predict(fit4, newdata = data.frame(college = c_val, female = f_val))
    label <- paste0(ifelse(f_val == 1, "Female", "Male"), ", ",
                    ifelse(c_val == 1, "College", "No College"))
    cat(sprintf("  %s: %.2f\n", label, yhat))
  }
}

# ============================================================================
# MODEL 5: FULL (news_hours + age + I(age^2) + college * female + income)
# ============================================================================

fit5 <- lm(knowledge ~ news_hours + age + I(age^2) + college * female + income, data = d)
cat("\n===== MODEL 5: FULL MODEL =====\n")
print(summary(fit5))

# ============================================================================
# R-SQUARED TABLE
# ============================================================================

cat("\n===== R-SQUARED COMPARISON =====\n")
r2 <- c(summary(fit1)$r.squared, summary(fit2)$r.squared,
        summary(fit3)$r.squared, summary(fit4)$r.squared,
        summary(fit5)$r.squared)
cat(sprintf("  Model 1 (news_hours):       R² = %.3f\n", r2[1]))
cat(sprintf("  Model 2 (+ age, female, college): R² = %.3f\n", r2[2]))
cat(sprintf("  Model 3 (age + age²):       R² = %.3f\n", r2[3]))
cat(sprintf("  Model 4 (college * female):  R² = %.3f\n", r2[4]))
cat(sprintf("  Model 5 (full):              R² = %.3f\n", r2[5]))

# ============================================================================
# TRAIN/TEST SPLIT
# ============================================================================

set.seed(51)
train_idx <- sample(1:nrow(d), size = 0.8 * nrow(d))
train <- d[train_idx, ]
test  <- d[-train_idx, ]

cat(sprintf("\nTrain: n = %d, Test: n = %d\n", nrow(train), nrow(test)))

# Fit all models on train, evaluate on test
models <- list(
  "1: news_hours"         = knowledge ~ news_hours,
  "2: + age, female, college" = knowledge ~ news_hours + age + female + college,
  "3: + age²"             = knowledge ~ news_hours + age + I(age^2) + female + college,
  "4: + college×female"   = knowledge ~ news_hours + age + I(age^2) + college * female,
  "5: + income (full)"    = knowledge ~ news_hours + age + I(age^2) + college * female + income
)

cat("\n===== TRAIN/TEST RMSE =====\n")
rmse_results <- tibble(model = character(), train_rmse = numeric(), test_rmse = numeric())

for (name in names(models)) {
  fit_train <- lm(models[[name]], data = train)

  yhat_train <- predict(fit_train, newdata = train)
  yhat_test  <- predict(fit_train, newdata = test)

  train_rmse <- sqrt(mean((train$knowledge - yhat_train)^2))
  test_rmse  <- sqrt(mean((test$knowledge  - yhat_test)^2))

  rmse_results <- bind_rows(rmse_results, tibble(model = name, train_rmse, test_rmse))
  cat(sprintf("  %s: Train = %.2f, Test = %.2f\n", name, train_rmse, test_rmse))
}

# ============================================================================
# FIGURES
# ============================================================================

# Scott's Gov 51 color palette
gov51_crimson <- "#A51C30"
gov51_blue    <- "#2980B9"
gov51_teal    <- "#16A085"
gov51_dark    <- "#1E3C72"
gov51_orange  <- "#E67E22"
gov51_gray    <- "#7F8C8D"

theme_gov51 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = gov51_dark, face = "bold", size = 16),
    axis.title = element_text(color = gov51_dark),
    axis.text = element_text(color = gov51_gray),
    panel.grid.minor = element_blank()
  )

# Figure 1: Histogram of knowledge scores
pdf("figures/fig_knowledge_hist.pdf", width = 8, height = 4.5)
ggplot(d, aes(x = knowledge)) +
  geom_histogram(binwidth = 2, fill = gov51_blue, color = "white", alpha = 0.8) +
  labs(x = "Civic Knowledge Score (0-100)", y = "Count",
       title = "Distribution of Civic Knowledge Scores") +
  geom_vline(xintercept = mean(d$knowledge), color = gov51_orange, linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = mean(d$knowledge) + 3, y = Inf, vjust = 2,
           label = sprintf("Mean = %.1f", mean(d$knowledge)),
           color = gov51_orange, fontface = "bold", size = 4.5) +
  theme_gov51
dev.off()

# Figure 2: Bivariate scatter (news_hours vs knowledge)
pdf("figures/fig_bivariate_scatter.pdf", width = 8, height = 5)
ggplot(d, aes(x = news_hours, y = knowledge)) +
  geom_point(alpha = 0.15, color = gov51_dark, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = gov51_blue, linewidth = 1.5) +
  labs(x = "Daily News Consumption (hours)", y = "Civic Knowledge Score",
       title = "News Consumption Predicts Civic Knowledge") +
  annotate("text", x = 5.5, y = 25,
           label = sprintf("slope = %.2f\nR² = %.3f", coef(fit1)[2], summary(fit1)$r.squared),
           color = gov51_blue, fontface = "bold", size = 4.5, hjust = 0) +
  theme_gov51
dev.off()

# Figure 3: Age polynomial (linear vs quadratic)
age_grid <- data.frame(age = seq(18, 85, by = 0.5))
fit_linear <- lm(knowledge ~ age, data = d)
age_grid$linear <- predict(fit_linear, newdata = age_grid)
age_grid$quadratic <- predict(fit3, newdata = age_grid)

pdf("figures/fig_age_polynomial.pdf", width = 8, height = 5)
ggplot(d, aes(x = age, y = knowledge)) +
  geom_point(alpha = 0.12, color = gov51_dark, size = 1.2) +
  geom_line(data = age_grid, aes(x = age, y = linear),
            color = gov51_gray, linewidth = 1.2, linetype = "dashed") +
  geom_line(data = age_grid, aes(x = age, y = quadratic),
            color = gov51_teal, linewidth = 1.5) +
  labs(x = "Age", y = "Civic Knowledge Score",
       title = "Age Has a Curved Relationship with Knowledge") +
  annotate("text", x = 74, y = age_grid$linear[age_grid$age == 74],
           label = "Linear", color = gov51_gray, fontface = "bold", size = 4, vjust = -1) +
  annotate("text", x = 74, y = age_grid$quadratic[age_grid$age == 74],
           label = "Quadratic", color = gov51_teal, fontface = "bold", size = 4, vjust = 1.5) +
  coord_cartesian(ylim = c(25, 60)) +
  theme_gov51
dev.off()

# Figure 4: Interaction plot (college x female - 4 group means)
group_means <- d %>%
  mutate(
    Gender = ifelse(female == 1, "Female", "Male"),
    Education = ifelse(college == 1, "College", "No College")
  ) %>%
  group_by(Gender, Education) %>%
  summarize(mean_knowledge = mean(knowledge), .groups = "drop")

pdf("figures/fig_interaction_plot.pdf", width = 8, height = 5)
ggplot(group_means, aes(x = Education, y = mean_knowledge, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f", mean_knowledge)),
            position = position_dodge(width = 0.7), vjust = -0.5,
            fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Male" = gov51_blue, "Female" = gov51_orange)) +
  labs(x = "", y = "Mean Civic Knowledge Score",
       title = "The College Effect Is Larger for Women") +
  coord_cartesian(ylim = c(0, max(group_means$mean_knowledge) + 5)) +
  theme_gov51 +
  theme(legend.position = "top", legend.title = element_blank())
dev.off()

# Figure 5: Train/Test RMSE comparison
rmse_long <- rmse_results %>%
  mutate(model_short = c("M1:\nnews", "M2:\n+age,fem,col", "M3:\n+age²",
                          "M4:\n+col×fem", "M5:\nfull")) %>%
  pivot_longer(cols = c(train_rmse, test_rmse),
               names_to = "set", values_to = "rmse") %>%
  mutate(set = ifelse(set == "train_rmse", "Train", "Test"))

pdf("figures/fig_traintest_rmse.pdf", width = 9, height = 5)
ggplot(rmse_long, aes(x = model_short, y = rmse, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f", rmse)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Train" = gov51_blue, "Test" = gov51_orange)) +
  labs(x = "", y = "RMSE (knowledge points)",
       title = "Train RMSE Always Improves — Test RMSE Is What Matters") +
  theme_gov51 +
  theme(legend.position = "top", legend.title = element_blank())
dev.off()

cat("\n===== ALL FIGURES SAVED =====\n")
cat("Done!\n")
