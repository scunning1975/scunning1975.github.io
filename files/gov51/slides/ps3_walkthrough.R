# ps3_walkthrough.R
# Gov 51: In-Class PS3 Walkthrough
# Run this live in class to show students the workflow
#
# This mirrors PS3 Q1-Q4 so students see every step before doing it themselves
# Keep the console visible — students learn from seeing the output

library(tidyverse)
library(glmnet)

# ============================================================================
# STEP 1: LOAD AND EXPLORE
# ============================================================================
cat("============ STEP 1: LOAD THE DATA ============\n\n")

compas <- read_csv("../../problem_sets/ps3/data/compas_clean.csv",
                    show_col_types = FALSE)
cat("Dimensions:", nrow(compas), "rows x", ncol(compas), "columns\n")
cat("Variables:", paste(names(compas), collapse = ", "), "\n\n")

# Recidivism rate
cat("Recidivism rate:", round(mean(compas$recidivism), 3), "\n")
cat("  (About", round(100 * mean(compas$recidivism)), "% were rearrested within 2 years)\n\n")

# ============================================================================
# STEP 2: TRAIN/TEST SPLIT
# ============================================================================
cat("============ STEP 2: TRAIN/TEST SPLIT ============\n\n")

set.seed(51)  # ALWAYS set the seed before anything random
n <- nrow(compas)
train_idx <- sample(1:n, size = floor(0.8 * n))
train <- compas[train_idx, ]
test  <- compas[-train_idx, ]
cat("Training:", nrow(train), "  Test:", nrow(test), "\n\n")

# ============================================================================
# STEP 3: OLS WITH MAIN EFFECTS
# ============================================================================
cat("============ STEP 3: SIMPLE OLS ============\n\n")

ols_simple <- lm(recidivism ~ ., data = train)

# How many coefficients?
cat("Coefficients (including intercept):", length(coef(ols_simple)), "\n")
cat("Predictors:", length(coef(ols_simple)) - 1, "\n\n")

# Which are significant?
s <- summary(ols_simple)
sig <- s$coefficients[s$coefficients[, 4] < 0.05, ]
cat("Significant predictors at 5%:\n")
print(round(sig, 4))

# RMSE
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

train_rmse <- rmse(train$recidivism, predict(ols_simple, train))
test_rmse  <- rmse(test$recidivism, predict(ols_simple, test))
cat("\nIn-sample RMSE: ", round(train_rmse, 4), "\n")
cat("Out-of-sample RMSE:", round(test_rmse, 4), "\n")
cat("Gap:", round(test_rmse - train_rmse, 4),
    "  <-- small gap means NOT much overfitting with 16 vars\n\n")

# ============================================================================
# STEP 4: THE KITCHEN SINK (INTERACTIONS)
# ============================================================================
cat("============ STEP 4: KITCHEN SINK ============\n\n")

f_full <- recidivism ~ (.)^2 +
  I(age^2) + I(priors_count^2) +
  I(juv_fel_count^2) + I(juv_misd_count^2) +
  I(juv_other_count^2)

# THIS is where model.matrix() comes in
X_train <- model.matrix(f_full, data = train)[, -1]
X_test  <- model.matrix(f_full, data = test)[, -1]
y_train <- train$recidivism
y_test  <- test$recidivism

cat("Features after interactions:", ncol(X_train), "\n")
cat("Training observations:", nrow(X_train), "\n")
cat("Ratio n/p:", round(nrow(X_train) / ncol(X_train), 1), "\n\n")

# OLS on the kitchen sink
ols_full <- lm(f_full, data = train)
full_train_rmse <- rmse(y_train, predict(ols_full, train))
full_test_rmse  <- rmse(y_test, predict(ols_full, test))
cat("Kitchen sink in-sample RMSE: ", round(full_train_rmse, 4), "\n")
cat("Kitchen sink out-of-sample RMSE:", round(full_test_rmse, 4), "\n")
cat("Gap:", round(full_test_rmse - full_train_rmse, 4),
    "  <-- BIGGER gap = overfitting!\n\n")

# ============================================================================
# STEP 5: RIDGE
# ============================================================================
cat("============ STEP 5: RIDGE (alpha = 0) ============\n\n")

set.seed(51)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)
cat("lambda.min:", round(cv_ridge$lambda.min, 6), "\n")
cat("lambda.1se:", round(cv_ridge$lambda.1se, 6), "\n")

ridge_pred <- predict(cv_ridge, s = "lambda.min", newx = X_test)
ridge_rmse <- rmse(y_test, ridge_pred)
cat("Ridge out-of-sample RMSE:", round(ridge_rmse, 4), "\n")

# Did Ridge zero anything out?
ridge_coefs <- coef(cv_ridge, s = "lambda.min")
cat("Non-zero coefficients:", sum(ridge_coefs[, 1] != 0) - 1,
    "of", ncol(X_train), " <-- Ridge never zeros anything out\n\n")

# ============================================================================
# STEP 6: LASSO
# ============================================================================
cat("============ STEP 6: LASSO (alpha = 1) ============\n\n")

set.seed(51)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)
cat("lambda.min:", round(cv_lasso$lambda.min, 6), "\n")

lasso_pred <- predict(cv_lasso, s = "lambda.min", newx = X_test)
lasso_rmse <- rmse(y_test, lasso_pred)
cat("LASSO out-of-sample RMSE:", round(lasso_rmse, 4), "\n")

lasso_coefs <- coef(cv_lasso, s = "lambda.min")
n_kept <- sum(lasso_coefs[, 1] != 0) - 1
cat("Variables kept:", n_kept, "of", ncol(X_train),
    " <-- LASSO eliminated", ncol(X_train) - n_kept, "variables!\n\n")

# Which survived?
nonzero <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
nonzero <- nonzero[nonzero != "(Intercept)"]
cat("Surviving variables:\n")
cat(paste(" ", nonzero), sep = "\n")

# ============================================================================
# STEP 7: ELASTIC NET
# ============================================================================
cat("\n============ STEP 7: ELASTIC NET (alpha = 0.5) ============\n\n")

set.seed(51)
cv_enet <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 10)
enet_pred <- predict(cv_enet, s = "lambda.min", newx = X_test)
enet_rmse <- rmse(y_test, enet_pred)
enet_coefs <- coef(cv_enet, s = "lambda.min")
enet_kept <- sum(enet_coefs[, 1] != 0) - 1
cat("Elastic Net RMSE:", round(enet_rmse, 4), "\n")
cat("Variables kept:", enet_kept, "\n\n")

# ============================================================================
# COMPARISON TABLE
# ============================================================================
cat("============ THE BIG TABLE ============\n\n")
cat(sprintf("%-30s %12s %18s\n", "Model", "Predictors", "Test RMSE"))
cat(sprintf("%-30s %12s %18s\n", "-----", "----------", "---------"))
cat(sprintf("%-30s %12d %18.4f\n", "OLS (16 main effects)", 16, test_rmse))
cat(sprintf("%-30s %12d %18.4f\n", "OLS (kitchen sink)", ncol(X_train), full_test_rmse))
cat(sprintf("%-30s %12d %18.4f\n", "Ridge", ncol(X_train), ridge_rmse))
cat(sprintf("%-30s %12d %18.4f\n", "LASSO", n_kept, lasso_rmse))
cat(sprintf("%-30s %12d %18.4f\n", "Elastic Net", enet_kept, enet_rmse))

cat("\n============ DONE ============\n")
cat("Now you do it on PS3! Same workflow, same data.\n")
