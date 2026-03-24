# week09_regularization.R
# Gov 51: Regularization Demo — LASSO, Ridge, Elastic Net
# Companion script for week09-regularization.tex
#
# Uses the same Ames Housing data as week06b_overfitting.R
# Demonstrates glmnet workflow students will use on PS3
#
# Requires: AmesHousing, tidyverse, glmnet

library(AmesHousing)
library(tidyverse)
library(glmnet)

set.seed(51)

# Colors (consistent with deck)
crimson <- "#A51C30"
navy <- "#1E3C72"
blue <- "#2980B9"
teal <- "#16A085"
gray_col <- "#7F8C8D"

theme_gov51 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = navy, face = "bold", size = 16),
    plot.subtitle = element_text(color = gray_col, size = 12),
    axis.title = element_text(color = navy),
    panel.grid.minor = element_blank()
  )

# Create figures directory if it doesn't exist
dir.create("figures", showWarnings = FALSE)

# ============================================================================
# DATA SETUP
# ============================================================================

ames <- make_ames()
cat("Ames Housing:", nrow(ames), "rows,", ncol(ames), "columns\n\n")

# Select numeric predictors only (for glmnet)
numeric_cols <- names(ames)[sapply(ames, is.numeric)]
predictors <- numeric_cols[numeric_cols != "Sale_Price"]
cat("Numeric predictors:", length(predictors), "\n")

# Train/test split (same as week06b)
n <- nrow(ames)
train_idx <- sample(1:n, size = floor(0.7 * n))
train <- ames[train_idx, ]
test <- ames[-train_idx, ]
cat("Train:", nrow(train), " Test:", nrow(test), "\n\n")

# Prepare matrices for glmnet (requires matrix input)
X_train <- as.matrix(train[, predictors])
Y_train <- train$Sale_Price
X_test <- as.matrix(test[, predictors])
Y_test <- test$Sale_Price

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# ============================================================================
# STEP 1: OLS BASELINE (for comparison)
# ============================================================================
cat("=== OLS BASELINE ===\n")
ols_fit <- lm(Sale_Price ~ ., data = train[, c("Sale_Price", predictors)])
ols_train_rmse <- rmse(Y_train, predict(ols_fit, train))
ols_test_rmse <- rmse(Y_test, predict(ols_fit, test))
cat("OLS Train RMSE: $", round(ols_train_rmse), "\n")
cat("OLS Test RMSE:  $", round(ols_test_rmse), "\n\n")

# ============================================================================
# STEP 2: RIDGE REGRESSION (alpha = 0)
# ============================================================================
cat("=== RIDGE REGRESSION ===\n")

# Cross-validation to find best lambda
cv_ridge <- cv.glmnet(X_train, Y_train, alpha = 0, nfolds = 10)
cat("Ridge lambda.min:", round(cv_ridge$lambda.min, 2), "\n")
cat("Ridge lambda.1se:", round(cv_ridge$lambda.1se, 2), "\n")

# Predict with lambda.min
ridge_pred_train <- predict(cv_ridge, X_train, s = "lambda.min")
ridge_pred_test <- predict(cv_ridge, X_test, s = "lambda.min")
cat("Ridge Train RMSE: $", round(rmse(Y_train, ridge_pred_train)), "\n")
cat("Ridge Test RMSE:  $", round(rmse(Y_test, ridge_pred_test)), "\n")

# How many nonzero coefficients? (Ridge: all of them)
ridge_coefs <- coef(cv_ridge, s = "lambda.min")
cat("Ridge nonzero coefficients:", sum(ridge_coefs != 0) - 1, "of",
    length(predictors), "\n\n")

# ============================================================================
# STEP 3: LASSO (alpha = 1)
# ============================================================================
cat("=== LASSO ===\n")

cv_lasso <- cv.glmnet(X_train, Y_train, alpha = 1, nfolds = 10)
cat("LASSO lambda.min:", round(cv_lasso$lambda.min, 2), "\n")
cat("LASSO lambda.1se:", round(cv_lasso$lambda.1se, 2), "\n")

# Predict with lambda.min
lasso_pred_train <- predict(cv_lasso, X_train, s = "lambda.min")
lasso_pred_test <- predict(cv_lasso, X_test, s = "lambda.min")
cat("LASSO Train RMSE: $", round(rmse(Y_train, lasso_pred_train)), "\n")
cat("LASSO Test RMSE:  $", round(rmse(Y_test, lasso_pred_test)), "\n")

# Which variables survived?
lasso_coefs <- coef(cv_lasso, s = "lambda.min")
lasso_nonzero <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
lasso_nonzero <- lasso_nonzero[lasso_nonzero != "(Intercept)"]
cat("LASSO nonzero coefficients:", length(lasso_nonzero), "of",
    length(predictors), "\n")
cat("Variables selected:", paste(lasso_nonzero, collapse = ", "), "\n\n")

# ============================================================================
# STEP 4: ELASTIC NET (alpha = 0.5)
# ============================================================================
cat("=== ELASTIC NET (alpha = 0.5) ===\n")

cv_enet <- cv.glmnet(X_train, Y_train, alpha = 0.5, nfolds = 10)
cat("Elastic Net lambda.min:", round(cv_enet$lambda.min, 2), "\n")

enet_pred_train <- predict(cv_enet, X_train, s = "lambda.min")
enet_pred_test <- predict(cv_enet, X_test, s = "lambda.min")
cat("Elastic Net Train RMSE: $", round(rmse(Y_train, enet_pred_train)), "\n")
cat("Elastic Net Test RMSE:  $", round(rmse(Y_test, enet_pred_test)), "\n")

enet_coefs <- coef(cv_enet, s = "lambda.min")
enet_nonzero <- sum(enet_coefs != 0) - 1
cat("Elastic Net nonzero coefficients:", enet_nonzero, "of",
    length(predictors), "\n\n")

# ============================================================================
# STEP 5: COMPARISON TABLE
# ============================================================================
cat("=== COMPARISON TABLE ===\n")
comparison <- data.frame(
  Method = c("OLS", "Ridge", "LASSO", "Elastic Net"),
  Train_RMSE = round(c(ols_train_rmse,
                        rmse(Y_train, ridge_pred_train),
                        rmse(Y_train, lasso_pred_train),
                        rmse(Y_train, enet_pred_train))),
  Test_RMSE = round(c(ols_test_rmse,
                       rmse(Y_test, ridge_pred_test),
                       rmse(Y_test, lasso_pred_test),
                       rmse(Y_test, enet_pred_test))),
  Nonzero = c(length(predictors),
               sum(ridge_coefs != 0) - 1,
               length(lasso_nonzero),
               enet_nonzero)
)
print(comparison)
cat("\n")

# ============================================================================
# FIGURE 1: CV ERROR CURVES
# ============================================================================

# LASSO CV curve
pdf("figures/lasso_cv_curve.pdf", width = 9, height = 5.5)
plot(cv_lasso, main = "LASSO: CV Error vs log(lambda)",
     col = crimson, lwd = 2)
abline(v = log(cv_lasso$lambda.min), col = blue, lty = 2, lwd = 2)
abline(v = log(cv_lasso$lambda.1se), col = teal, lty = 2, lwd = 2)
legend("topleft",
       legend = c("lambda.min", "lambda.1se"),
       col = c(blue, teal), lty = 2, lwd = 2)
dev.off()
cat("Figure saved: figures/lasso_cv_curve.pdf\n")

# ============================================================================
# FIGURE 2: COEFFICIENT PATHS (LASSO)
# ============================================================================

# Fit full LASSO path
lasso_path <- glmnet(X_train, Y_train, alpha = 1)

pdf("figures/lasso_coef_paths.pdf", width = 9, height = 5.5)
plot(lasso_path, xvar = "lambda", label = TRUE,
     main = "LASSO Coefficient Paths",
     col = rainbow(length(predictors)),
     lwd = 1.5)
abline(v = log(cv_lasso$lambda.min), col = crimson, lty = 2, lwd = 2)
dev.off()
cat("Figure saved: figures/lasso_coef_paths.pdf\n")

# ============================================================================
# FIGURE 3: RMSE COMPARISON BAR CHART
# ============================================================================

p_compare <- ggplot(comparison, aes(x = reorder(Method, Test_RMSE),
                                     y = Test_RMSE)) +
  geom_col(fill = c(crimson, teal, blue, navy), width = 0.6) +
  geom_text(aes(label = paste0("$", scales::comma(Test_RMSE))),
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(x = "", y = "Test RMSE ($)",
       title = "Test RMSE: Penalized methods beat OLS") +
  scale_y_continuous(labels = scales::dollar_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  theme_gov51

ggsave("figures/rmse_comparison.pdf", p_compare, width = 8, height = 5)
cat("Figure saved: figures/rmse_comparison.pdf\n")

# ============================================================================
# STEP 6: PREDICTION INTERVALS (OLS only — clean formula)
# ============================================================================
cat("\n=== PREDICTION INTERVALS ===\n")
cat("Prediction intervals only work for OLS — LASSO/Ridge don't have them.\n")
cat("This connects to the iid sampling assumption we learned in weeks 3-6.\n\n")

# Use the 20-variable OLS model (good balance of fit and interpretability)
ols_20 <- lm(fmla20, data = train)

# Pick a specific house from the test set to predict
example_house <- test[42, ]  # just pick one
cat("Example house: Gr_Liv_Area =", example_house$Gr_Liv_Area,
    " Year_Built =", example_house$Year_Built,
    " Overall_Qual =", example_house$Overall_Qual, "\n")
cat("Actual Sale Price: $", scales::comma(example_house$Sale_Price), "\n\n")

# Confidence interval (for the MEAN of houses like this)
ci <- predict(ols_20, newdata = example_house, interval = "confidence", level = 0.95)
cat("--- Confidence Interval (where is the AVERAGE house like this?) ---\n")
cat("  Point prediction: $", scales::comma(round(ci[1, "fit"])), "\n")
cat("  95% CI: ($", scales::comma(round(ci[1, "lwr"])),
    ", $", scales::comma(round(ci[1, "upr"])), ")\n")
cat("  CI width: $", scales::comma(round(ci[1, "upr"] - ci[1, "lwr"])), "\n\n")

# Prediction interval (for THIS SPECIFIC house)
pi <- predict(ols_20, newdata = example_house, interval = "prediction", level = 0.95)
cat("--- Prediction Interval (where will THIS house actually sell?) ---\n")
cat("  Point prediction: $", scales::comma(round(pi[1, "fit"])), "\n")
cat("  95% PI: ($", scales::comma(round(pi[1, "lwr"])),
    ", $", scales::comma(round(pi[1, "upr"])), ")\n")
cat("  PI width: $", scales::comma(round(pi[1, "upr"] - pi[1, "lwr"])), "\n\n")

cat("KEY INSIGHT: Same point prediction, but the PI is MUCH wider.\n")
cat("CI width:  $", scales::comma(round(ci[1, "upr"] - ci[1, "lwr"])), "\n")
cat("PI width:  $", scales::comma(round(pi[1, "upr"] - pi[1, "lwr"])), "\n")
cat("The extra width is the IRREDUCIBLE NOISE — even if we knew beta\n")
cat("perfectly, this specific house has its own epsilon (iid sampling).\n\n")

# ============================================================================
# FIGURE 4: CI vs PI BANDS
# ============================================================================

# Simple regression for clean visual
ols_simple <- lm(Sale_Price ~ Gr_Liv_Area, data = train)

# Generate predictions over a grid
grid <- data.frame(Gr_Liv_Area = seq(500, 3500, length.out = 200))
ci_grid <- predict(ols_simple, newdata = grid, interval = "confidence")
pi_grid <- predict(ols_simple, newdata = grid, interval = "prediction")

band_data <- data.frame(
  sqft = grid$Gr_Liv_Area,
  fit = ci_grid[, "fit"],
  ci_lwr = ci_grid[, "lwr"], ci_upr = ci_grid[, "upr"],
  pi_lwr = pi_grid[, "lwr"], pi_upr = pi_grid[, "upr"]
)

# Random subsample for plotting (don't crowd the chart)
plot_sub <- test[sample(nrow(test), 200), ]

p_intervals <- ggplot() +
  # Prediction interval band (wider, light)
  geom_ribbon(data = band_data, aes(x = sqft, ymin = pi_lwr, ymax = pi_upr),
              fill = crimson, alpha = 0.12) +
  # Confidence interval band (narrow, darker)
  geom_ribbon(data = band_data, aes(x = sqft, ymin = ci_lwr, ymax = ci_upr),
              fill = blue, alpha = 0.25) +
  # Regression line
  geom_line(data = band_data, aes(x = sqft, y = fit),
            color = navy, linewidth = 1.2) +
  # Test data points
  geom_point(data = plot_sub, aes(x = Gr_Liv_Area, y = Sale_Price),
             alpha = 0.3, color = gray_col, size = 1.5) +
  # Labels
  annotate("text", x = 3200, y = band_data$ci_upr[190] + 10000,
           label = "95% CI", color = blue, fontface = "bold", size = 4.5) +
  annotate("text", x = 3200, y = band_data$pi_upr[190] + 10000,
           label = "95% PI", color = crimson, fontface = "bold", size = 4.5) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_cartesian(ylim = c(-50000, 600000)) +
  labs(x = "Square Footage (Gr_Liv_Area)",
       y = "Sale Price ($)",
       title = "The prediction interval is always wider than the confidence interval",
       subtitle = "CI: where is the average?  PI: where will this specific house sell?") +
  theme_gov51

ggsave("figures/ci_vs_pi.pdf", p_intervals, width = 10, height = 6)
cat("Figure saved: figures/ci_vs_pi.pdf\n")

# ============================================================================
# BONUS: Show PI width varies with distance from mean
# ============================================================================
cat("\n--- PI width varies by distance from x-bar ---\n")
cat("x-bar (Gr_Liv_Area) =", round(mean(train$Gr_Liv_Area)), "sqft\n\n")

example_points <- data.frame(
  Gr_Liv_Area = c(1000, 1500, 2000, 2500, 3000),
  label = c("small", "below avg", "near avg", "above avg", "large")
)

for (i in 1:nrow(example_points)) {
  pi_i <- predict(ols_simple, newdata = example_points[i, , drop = FALSE],
                  interval = "prediction")
  width_i <- pi_i[1, "upr"] - pi_i[1, "lwr"]
  cat("  ", example_points$label[i], "(", example_points$Gr_Liv_Area[i], "sqft):",
      "PI width = $", scales::comma(round(width_i)), "\n")
}
cat("\nKey: PI is narrowest near x-bar, wider at extremes (extrapolation penalty).\n")
cat("This is the (x0 - xbar)^2 / sum(xi - xbar)^2 term in the formula.\n")

cat("\n=== DONE ===\n")
cat("All figures saved to figures/\n")
cat("Students: modify this script for PS3 by replacing Ames data with COMPAS\n")
