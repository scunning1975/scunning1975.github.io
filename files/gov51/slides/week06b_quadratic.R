# week06b_quadratic.R
# Quadratic experience model and predicted wage curve

library(wooldridge)
library(tidyverse)

data(wage1)

# Colors
crimson <- "#A51C30"
navy <- "#1E3C72"
blue <- "#2980B9"
gray_col <- "#7F8C8D"

theme_gov51 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(color = navy, face = "bold", size = 16),
    plot.subtitle = element_text(color = gray_col, size = 12),
    axis.title = element_text(color = navy),
    panel.grid.minor = element_blank()
  )

# Model 2b: quadratic in experience
m2b <- lm(wage ~ female + educ + exper + I(exper^2) + tenure, data = wage1)

cat("=== Quadratic Model ===\n")
print(summary(m2b))

coefs <- coef(m2b)
cat("\nCoefficients:\n")
cat("  Intercept:", round(coefs["(Intercept)"], 4), "\n")
cat("  female:", round(coefs["female"], 4), "\n")
cat("  educ:", round(coefs["educ"], 4), "\n")
cat("  exper:", round(coefs["exper"], 4), "\n")
cat("  exper^2:", round(coefs["I(exper^2)"], 4), "\n")
cat("  tenure:", round(coefs["tenure"], 4), "\n")

# Marginal effect: d(wage)/d(exper) = beta_exper + 2*beta_exper2 * exper
b_exper <- coefs["exper"]
b_exper2 <- coefs["I(exper^2)"]

marginal_effects <- data.frame(
  exper = c(5, 15, 25, 35),
  me = b_exper + 2 * b_exper2 * c(5, 15, 25, 35)
)
cat("\nMarginal effect of experience:\n")
print(round(marginal_effects, 4))

# Turning point
turning_point <- -b_exper / (2 * b_exper2)
cat("\nTurning point: exper =", round(turning_point, 1), "years\n")

# Figure: predicted wage vs experience (holding female=0, educ=12.5, tenure=5)
exper_seq <- seq(0, 50, by = 0.5)
pred_data <- data.frame(
  female = 0, educ = 12.5, exper = exper_seq,
  tenure = 5
)
pred_data$wage_hat <- predict(m2b, newdata = pred_data)

p <- ggplot(pred_data, aes(x = exper, y = wage_hat)) +
  geom_line(color = blue, linewidth = 1.5) +
  geom_vline(xintercept = turning_point, linetype = "dashed", color = crimson,
             linewidth = 1) +
  annotate("text", x = turning_point + 1.5, y = max(pred_data$wage_hat) - 0.3,
           label = paste0("Peak: ", round(turning_point, 0), " years"),
           color = crimson, fontface = "bold", size = 5, hjust = 0) +
  labs(x = "Years of Experience",
       y = "Predicted Wage ($/hr)",
       title = "Experience has diminishing returns",
       subtitle = "Male, 12.5 yrs education, 5 yrs tenure") +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  theme_gov51

ggsave("figures/wage_quadratic_exper.pdf", p, width = 9, height = 5)
cat("\nFigure saved: figures/wage_quadratic_exper.pdf\n")

# Also extract SEs for the table
se <- summary(m2b)$coefficients[, 2]
cat("\nStandard errors:\n")
cat("  exper:", round(se["exper"], 4), "\n")
cat("  exper^2:", round(se["I(exper^2)"], 6), "\n")
cat("  R-squared:", round(summary(m2b)$r.squared, 3), "\n")
