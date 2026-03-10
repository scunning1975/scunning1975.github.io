# week06b_overfitting.R
# Ames Housing: demonstrate overfitting
# Two approaches:
#   1) Polynomial fits on small subsample (VISUAL: wiggly curves)
#   2) Multi-variable models on full data (QUANTITATIVE: RMSE table + curve)

library(AmesHousing)
library(tidyverse)

set.seed(51)

# Colors
crimson <- "#A51C30"
navy <- "#1E3C72"
blue <- "#2980B9"
gray_col <- "#7F8C8D"
teal <- "#16A085"
orange <- "#E67E22"

theme_gov51 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = navy, face = "bold", size = 16),
    plot.subtitle = element_text(color = gray_col, size = 12),
    axis.title = element_text(color = navy),
    panel.grid.minor = element_blank()
  )

# Load and clean
ames <- make_ames()
cat("Ames:", nrow(ames), "rows,", ncol(ames), "columns\n")

# Full train/test split: 70/30
n <- nrow(ames)
train_idx <- sample(1:n, size = floor(0.7 * n))
train <- ames[train_idx, ]
test <- ames[-train_idx, ]
cat("Train:", nrow(train), " Test:", nrow(test), "\n")

# Helper functions
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}
r2 <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

# ============================================================================
# PART 1: POLYNOMIAL FITS VISUAL (small subsample)
# This is purely for the visual — showing wiggly curves on a scatter plot
# ============================================================================
sub_idx <- sample(1:nrow(train), 60)
train_sub <- train[sub_idx, ]
cat("\nPolynomial demo subsample:", nrow(train_sub), "houses\n")

degrees <- c(1, 2, 4, 10)
poly_models <- list()

for (d in degrees) {
  fmla <- as.formula(paste0("Sale_Price ~ poly(Gr_Liv_Area, ", d, ")"))
  poly_models[[as.character(d)]] <- lm(fmla, data = train_sub)
}

# Train R-squared on the subsample (to show it climbs)
cat("\n=== POLYNOMIAL TRAIN R-SQUARED (60 houses) ===\n")
for (d in degrees) {
  m <- poly_models[[as.character(d)]]
  tr <- round(r2(train_sub$Sale_Price, predict(m, train_sub)), 3)
  cat("Degree", d, ": Train R2 =", tr, "\n")
}

# Generate smooth prediction curves within training range only
sqft_min <- min(train_sub$Gr_Liv_Area)
sqft_max <- max(train_sub$Gr_Liv_Area)
sqft_range <- seq(sqft_min, sqft_max, length.out = 500)
pred_grid <- data.frame(Gr_Liv_Area = sqft_range)

curve_data <- bind_rows(lapply(degrees, function(d) {
  preds <- predict(poly_models[[as.character(d)]], pred_grid)
  data.frame(sqft = sqft_range, price = preds,
             degree = paste0("Degree ", d))
}))
curve_data$degree <- factor(curve_data$degree,
  levels = paste0("Degree ", degrees))

# Clamp for clean visualization
y_upper <- max(train_sub$Sale_Price) * 1.5
y_lower <- min(train_sub$Sale_Price) * 0.3

p_poly <- ggplot() +
  geom_point(data = train_sub, aes(x = Gr_Liv_Area, y = Sale_Price),
             alpha = 0.6, color = navy, size = 2.5) +
  geom_line(data = curve_data, aes(x = sqft, y = price),
            color = crimson, linewidth = 1.3) +
  facet_wrap(~degree, ncol = 2) +
  coord_cartesian(ylim = c(y_lower, y_upper)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Square Footage",
       y = "Sale Price ($)",
       title = "Same 60 houses, four polynomial fits") +
  theme_gov51 +
  theme(strip.text = element_text(face = "bold", size = 13, color = navy))

ggsave("figures/ames_poly_fits.pdf", p_poly, width = 10, height = 7)
cat("\nFigure saved: figures/ames_poly_fits.pdf\n")


# ============================================================================
# PART 2: MULTI-VARIABLE MODELS (full data)
# This is for the RMSE table + overfitting curve
# ============================================================================
numeric_cols <- names(ames)[sapply(ames, is.numeric)]
all_numeric <- numeric_cols[numeric_cols != "Sale_Price"]

# Model 1: 1 variable
m1 <- lm(Sale_Price ~ Gr_Liv_Area, data = train)

# Model 2: 4 variables
m4 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Overall_Qual + Garage_Area,
          data = train)

# Model 3: 20 variables
num_vars_20 <- c("Gr_Liv_Area", "Year_Built", "Overall_Qual", "Garage_Area",
                  "Total_Bsmt_SF", "First_Flr_SF", "Full_Bath", "Year_Remod_Add",
                  "Lot_Area", "TotRms_AbvGrd", "Fireplaces", "BsmtFin_SF_1",
                  "Second_Flr_SF", "Open_Porch_SF", "Wood_Deck_SF", "Half_Bath",
                  "Bedroom_AbvGr", "Bsmt_Full_Bath", "Screen_Porch", "Pool_Area")
fmla20 <- as.formula(paste("Sale_Price ~", paste(num_vars_20, collapse = " + ")))
m20 <- lm(fmla20, data = train)

# Model 4: kitchen sink — all numeric + squares + pairwise interactions of top 20
poly_terms <- paste0("I(", all_numeric, "^2)")
int_terms <- combn(all_numeric[1:20], 2, FUN = function(x) paste(x, collapse = ":"))
fmla_all <- as.formula(paste("Sale_Price ~",
  paste(c(all_numeric, poly_terms, int_terms), collapse = " + ")))
m_all <- lm(fmla_all, data = train)
n_coeffs <- length(coef(m_all))
cat("\nKitchen sink model coefficients:", n_coeffs, "\n")

# Results table
cat("\n=== MULTI-VARIABLE RESULTS ===\n")
models <- list(m1, m4, m20, m_all)
model_names <- c(1, 4, 20, n_coeffs)

multi_results <- data.frame(
  Variables = model_names,
  Train_R2 = sapply(models, function(m) round(r2(train$Sale_Price, predict(m, train)), 3)),
  Test_R2 = sapply(models, function(m) round(r2(test$Sale_Price, predict(m, test)), 3)),
  Train_RMSE = sapply(models, function(m) round(rmse(train$Sale_Price, predict(m, train)))),
  Test_RMSE = sapply(models, function(m) round(rmse(test$Sale_Price, predict(m, test))))
)
print(multi_results)

# ---- Figure 3: Predicted vs Actual scatter (2-panel, fixed scales) ----
test_pred4 <- predict(m4, test)
test_pred_all <- predict(m_all, test)

scatter_data <- bind_rows(
  data.frame(actual = test$Sale_Price, predicted = test_pred4,
             model = "4 variables"),
  data.frame(actual = test$Sale_Price, predicted = test_pred_all,
             model = paste0(n_coeffs, " variables"))
)
scatter_data$model <- factor(scatter_data$model,
  levels = c("4 variables", paste0(n_coeffs, " variables")))

# Use fixed scales but clip extreme outliers for readability
clip_upper <- quantile(test_pred_all, 0.995, na.rm = TRUE)
clip_lower <- quantile(test_pred_all, 0.005, na.rm = TRUE)
scatter_clipped <- scatter_data %>%
  filter(predicted >= clip_lower & predicted <= clip_upper)

p_scatter <- ggplot(scatter_clipped, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3, color = blue, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = crimson, linewidth = 1,
              linetype = "dashed") +
  facet_wrap(~model) +
  labs(x = "Actual Sale Price ($)",
       y = "Predicted Sale Price ($)",
       title = "Predicted vs Actual on TEST data") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_gov51 +
  theme(strip.text = element_text(face = "bold", size = 14, color = navy))

ggsave("figures/ames_predictions_scatter.pdf", p_scatter, width = 12, height = 5.5)
cat("\nFigure saved: figures/ames_predictions_scatter.pdf\n")

# ---- Figure 4: Overfitting curve (RMSE vs # predictors) ----
sizes <- c(1, 2, 3, 4, 6, 8, 10, 15, 20)
curve_data_mv <- data.frame(nvars = integer(), train_rmse = numeric(),
                              test_rmse = numeric())

for (k in sizes) {
  vars_k <- all_numeric[1:min(k, length(all_numeric))]
  fmla_k <- as.formula(paste("Sale_Price ~", paste(vars_k, collapse = " + ")))
  mk <- lm(fmla_k, data = train)
  curve_data_mv <- rbind(curve_data_mv, data.frame(
    nvars = k,
    train_rmse = rmse(train$Sale_Price, predict(mk, train)),
    test_rmse = rmse(test$Sale_Price, predict(mk, test))
  ))
}

# Add the kitchen sink
curve_data_mv <- rbind(curve_data_mv, data.frame(
  nvars = n_coeffs,
  train_rmse = rmse(train$Sale_Price, predict(m_all, train)),
  test_rmse = rmse(test$Sale_Price, predict(m_all, test))
))

curve_long <- curve_data_mv %>%
  pivot_longer(cols = c(train_rmse, test_rmse),
               names_to = "type", values_to = "rmse") %>%
  mutate(type = ifelse(type == "train_rmse", "Train RMSE", "Test RMSE"))

p_rmse <- ggplot(curve_long, aes(x = nvars, y = rmse, color = type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Train RMSE" = blue, "Test RMSE" = crimson)) +
  labs(x = "Number of Predictors",
       y = "RMSE ($)",
       title = "Training error always falls; test error eventually rises",
       color = "") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_gov51 +
  theme(legend.position = c(0.7, 0.8),
        legend.text = element_text(size = 13))

ggsave("figures/ames_overfitting_curve.pdf", p_rmse, width = 9, height = 5.5)
cat("Figure saved: figures/ames_overfitting_curve.pdf\n")
