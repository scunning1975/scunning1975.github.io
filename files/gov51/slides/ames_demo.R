# ames_demo.R
# Gov 51: In-Class Application — Predicting House Prices with Regularization
# Ames Housing dataset: 2,930 houses sold in Ames, Iowa (2006-2010)
# Same workflow you'll apply on your own data

library(tidyverse)
library(glmnet)
library(AmesHousing)

# ============================================================================
# STEP 1: LOAD AND EXPLORE
# ============================================================================
cat("============ STEP 1: THE DATA ============\n\n")

ames <- make_ames() %>%
  mutate(
    Overall_Qual = as.numeric(Overall_Qual),
    Overall_Cond = as.numeric(Overall_Cond)
  )

cat("Dimensions:", nrow(ames), "rows x", ncol(ames), "columns\n")
cat("Outcome: Sale_Price  (median = $", format(median(ames$Sale_Price), big.mark=","), ")\n\n")

cat("A few predictors:\n")
cat("  Gr_Liv_Area   -- above-ground living area (sq ft)\n")
cat("  Overall_Qual  -- overall material quality (1-10)\n")
cat("  Year_Built    -- original construction date\n")
cat("  Neighborhood  -- 28 neighborhoods in Ames\n")
cat("  Garage_Area   -- garage size (sq ft)\n\n")

# ============================================================================
# STEP 2: TRAIN/TEST SPLIT
# ============================================================================
cat("============ STEP 2: TRAIN/TEST SPLIT ============\n\n")

set.seed(51)
n <- nrow(ames)
train_idx <- sample(1:n, size = floor(0.8 * n))
train <- ames[train_idx, ]
test  <- ames[-train_idx, ]
cat("Training:", nrow(train), "  Test:", nrow(test), "\n\n")

# ============================================================================
# STEP 3: SIMPLE OLS — KEY PREDICTORS ONLY
# ============================================================================
cat("============ STEP 3: SIMPLE OLS ============\n\n")

key_vars <- c("Sale_Price", "Gr_Liv_Area", "Lot_Area", "Year_Built",
              "Overall_Qual", "Overall_Cond", "Total_Bsmt_SF",
              "Garage_Area", "Full_Bath", "Bedroom_AbvGr",
              "TotRms_AbvGrd", "Fireplaces", "Year_Remod_Add",
              "Neighborhood", "House_Style", "Bldg_Type")

train_s <- train[, key_vars]
test_s  <- test[,  key_vars]

ols_simple <- lm(Sale_Price ~ ., data = train_s)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

train_rmse <- rmse(train_s$Sale_Price, predict(ols_simple, train_s))
test_rmse  <- rmse(test_s$Sale_Price,  predict(ols_simple, test_s))

cat("Coefficients (incl. intercept):", length(coef(ols_simple)), "\n")
cat("In-sample RMSE: ", paste0("$", format(round(train_rmse), big.mark=",")), "\n")
cat("Out-of-sample RMSE:", paste0("$", format(round(test_rmse), big.mark=",")), "\n")
cat("Gap:", paste0("$", format(round(test_rmse - train_rmse), big.mark=",")),
    " <-- modest: not much overfitting yet\n\n")

# ============================================================================
# STEP 4: THE KITCHEN SINK (ALL NUMERIC INTERACTIONS)
# ============================================================================
cat("============ STEP 4: KITCHEN SINK ============\n\n")

# Use all numeric predictors + their pairwise interactions
# THIS is where model.matrix() becomes essential
num_preds <- names(ames)[sapply(ames, is.numeric)]
num_preds <- num_preds[num_preds != "Sale_Price"]

# Drop near-zero variance columns in training set
good_preds <- num_preds[sapply(train[, num_preds], var, na.rm = TRUE) > 0]

f_full <- as.formula(
  paste("Sale_Price ~ (", paste(good_preds, collapse = " + "), ")^2")
)

X_train <- model.matrix(f_full, data = train)[, -1]
X_test  <- model.matrix(f_full, data = test)[, -1]

# Align columns (test may have same cols as train after matrix construction)
common_cols <- intersect(colnames(X_train), colnames(X_test))
X_train <- X_train[, common_cols]
X_test  <- X_test[,  common_cols]

y_train <- train$Sale_Price
y_test  <- test$Sale_Price

cat("Features after interactions:", ncol(X_train), "\n")
cat("Training observations:", nrow(X_train), "\n")
cat("Ratio n/p:", round(nrow(X_train) / ncol(X_train), 1), "\n\n")

ols_full <- lm(f_full, data = train)
full_train_rmse <- rmse(y_train, predict(ols_full, train))
full_test_rmse  <- rmse(y_test,  predict(ols_full, test))

cat("Kitchen sink in-sample RMSE: ", paste0("$", format(round(full_train_rmse), big.mark=",")), "\n")
cat("Kitchen sink out-of-sample RMSE:", paste0("$", format(round(full_test_rmse), big.mark=",")), "\n")
cat("Gap:", paste0("$", format(round(full_test_rmse - full_train_rmse), big.mark=",")),
    " <-- BIGGER gap = overfitting!\n\n")

# ============================================================================
# STEP 5: RIDGE
# ============================================================================
cat("============ STEP 5: RIDGE (alpha = 0) ============\n\n")

set.seed(51)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)
cat("Optimal lambda (lambda.min):", round(cv_ridge$lambda.min), "\n")

ridge_pred <- predict(cv_ridge, s = "lambda.min", newx = X_test)
ridge_rmse <- rmse(y_test, ridge_pred)
cat("Ridge out-of-sample RMSE:", paste0("$", format(round(ridge_rmse), big.mark=",")), "\n")

ridge_coefs <- coef(cv_ridge, s = "lambda.min")
cat("Non-zero coefficients:", sum(ridge_coefs[, 1] != 0) - 1,
    "of", ncol(X_train), " <-- Ridge never zeros anything out\n\n")

# ============================================================================
# STEP 6: LASSO
# ============================================================================
cat("============ STEP 6: LASSO (alpha = 1) ============\n\n")

set.seed(51)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)
cat("Optimal lambda (lambda.min):", round(cv_lasso$lambda.min), "\n")

lasso_pred <- predict(cv_lasso, s = "lambda.min", newx = X_test)
lasso_rmse <- rmse(y_test, lasso_pred)
cat("LASSO out-of-sample RMSE:", paste0("$", format(round(lasso_rmse), big.mark=",")), "\n")

lasso_coefs <- coef(cv_lasso, s = "lambda.min")
n_kept <- sum(lasso_coefs[, 1] != 0) - 1
cat("Variables kept:", n_kept, "of", ncol(X_train),
    " -- LASSO eliminated", ncol(X_train) - n_kept, "\n\n")

nonzero_names <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
nonzero_names <- nonzero_names[nonzero_names != "(Intercept)"]
top_coefs <- sort(abs(lasso_coefs[nonzero_names, 1]), decreasing = TRUE)[1:10]
cat("Top 10 surviving variables (by coefficient magnitude):\n")
print(round(top_coefs))

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

cat("Elastic Net RMSE:", paste0("$", format(round(enet_rmse), big.mark=",")), "\n")
cat("Variables kept:", enet_kept, "\n\n")

# ============================================================================
# COMPARISON TABLE
# ============================================================================
cat("============ THE COMPARISON TABLE ============\n\n")
cat(sprintf("%-35s %12s %20s\n", "Model", "Predictors", "Test RMSE"))
cat(sprintf("%-35s %12s %20s\n", "-----", "----------", "---------"))
cat(sprintf("%-35s %12d %20s\n", "OLS (main effects)",
    length(coef(ols_simple)) - 1,
    paste0("$", format(round(test_rmse), big.mark=","))))
cat(sprintf("%-35s %12d %20s\n", "OLS (kitchen sink)",
    ncol(X_train),
    paste0("$", format(round(full_test_rmse), big.mark=","))))
cat(sprintf("%-35s %12d %20s\n", "Ridge",
    ncol(X_train),
    paste0("$", format(round(ridge_rmse), big.mark=","))))
cat(sprintf("%-35s %12d %20s\n", "LASSO",
    n_kept,
    paste0("$", format(round(lasso_rmse), big.mark=","))))
cat(sprintf("%-35s %12d %20s\n", "Elastic Net",
    enet_kept,
    paste0("$", format(round(enet_rmse), big.mark=","))))

cat("\n============ DONE ============\n")
