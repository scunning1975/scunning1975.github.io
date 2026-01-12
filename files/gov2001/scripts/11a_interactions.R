# =============================================================================
# Gov 2001: Lecture 11a - Interactions and Nonlinearities
# Spring 2026
# =============================================================================

# This script demonstrates interaction terms, log transformations, and
# polynomial regression in R.
# Key insight: Interactions let effects vary by group or level of another variable.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2001)

# -----------------------------------------------------------------------------
# Part 1: Interaction with Binary Variable
# -----------------------------------------------------------------------------

cat("=== Interaction: Binary x Continuous ===\n\n")

# Simulate campaign data
n <- 200

# Generate data
competitive <- rbinom(n, 1, 0.5)  # Binary: competitive race or not
ads <- runif(n, 0, 10)            # Continuous: number of ads (0-10)

# True model: interaction effect
# Effect of ads is larger in competitive races
vote_share <- 45 + 0.8 * ads + 5 * competitive + 1.2 * ads * competitive +
              rnorm(n, sd = 3)

campaign <- data.frame(vote_share, ads, competitive)

# Fit interaction model
m_interact <- lm(vote_share ~ ads * competitive, data = campaign)

cat("Interaction model: vote_share ~ ads * competitive\n\n")
cat("Coefficients:\n")
print(round(coef(m_interact), 3))

# Interpret the coefficients
cat("\nInterpretation:\n")
cat("  Intercept:         ", round(coef(m_interact)[1], 2),
    " (baseline vote share, safe seat, no ads)\n")
cat("  ads:               ", round(coef(m_interact)["ads"], 2),
    " (effect of ads in SAFE seats)\n")
cat("  competitive:       ", round(coef(m_interact)["competitive"], 2),
    " (difference in intercept for competitive races)\n")
cat("  ads:competitive:   ", round(coef(m_interact)["ads:competitive"], 2),
    " (ADDITIONAL effect of ads in competitive races)\n")

# Group-specific slopes
slope_safe <- coef(m_interact)["ads"]
slope_competitive <- coef(m_interact)["ads"] + coef(m_interact)["ads:competitive"]

cat("\nGroup-specific slopes:\n")
cat("  Effect of ads in SAFE seats:       ", round(slope_safe, 2), "\n")
cat("  Effect of ads in COMPETITIVE seats:", round(slope_competitive, 2), "\n")

# -----------------------------------------------------------------------------
# Part 2: Visualizing the Interaction
# -----------------------------------------------------------------------------

cat("\n=== Visualizing the Interaction ===\n")

par(mfrow = c(1, 2))

# Plot 1: Raw data by group
plot(ads[competitive == 0], vote_share[competitive == 0],
     pch = 16, col = "steelblue", xlim = c(0, 10), ylim = c(40, 75),
     xlab = "Number of Ads", ylab = "Vote Share (%)",
     main = "Campaign Ads and Vote Share")
points(ads[competitive == 1], vote_share[competitive == 1],
       pch = 17, col = "coral")

# Add regression lines
abline(a = coef(m_interact)[1],
       b = coef(m_interact)["ads"],
       col = "steelblue", lwd = 2)
abline(a = coef(m_interact)[1] + coef(m_interact)["competitive"],
       b = slope_competitive,
       col = "coral", lwd = 2)

legend("topleft",
       legend = c("Safe Seats", "Competitive"),
       col = c("steelblue", "coral"),
       pch = c(16, 17), lwd = 2, bty = "n")

# Plot 2: Marginal effects
barplot(c(slope_safe, slope_competitive),
        names.arg = c("Safe", "Competitive"),
        col = c("steelblue", "coral"),
        main = "Effect of Ads by District Type",
        ylab = "Change in Vote Share per Ad",
        ylim = c(0, 2.5))

par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# Part 3: Continuous x Continuous Interaction
# -----------------------------------------------------------------------------

cat("\n=== Interaction: Continuous x Continuous ===\n\n")

# Simulate: education x experience on wages
n <- 500
education <- rnorm(n, mean = 14, sd = 2)
experience <- runif(n, 0, 30)

# Interaction: education matters more when you have experience
log_wage <- 2 + 0.08 * education + 0.03 * experience +
            0.005 * education * experience + rnorm(n, sd = 0.3)

m_cont <- lm(log_wage ~ education * experience)

cat("Model: log_wage ~ education * experience\n\n")
cat("Coefficients:\n")
print(round(coef(m_cont), 4))

# Marginal effect of education depends on experience
cat("\nMarginal effect of education = ", round(coef(m_cont)["education"], 4),
    " + ", round(coef(m_cont)["education:experience"], 4),
    " * experience\n", sep = "")

cat("\nEffect of one more year of education:\n")
for (exp in c(0, 10, 20, 30)) {
  effect <- coef(m_cont)["education"] +
            coef(m_cont)["education:experience"] * exp
  cat("  At experience =", exp, ": ", round(effect, 4),
      " (", round(100 * effect, 1), "% wage increase)\n", sep = "")
}

# -----------------------------------------------------------------------------
# Part 4: Centering Variables
# -----------------------------------------------------------------------------

cat("\n=== Centering Variables ===\n\n")

# Without centering, beta_1 is effect when X_2 = 0
# Center at mean for more interpretable coefficients

education_c <- education - mean(education)
experience_c <- experience - mean(experience)

m_centered <- lm(log_wage ~ education_c * experience_c)

cat("After centering at means:\n")
cat("  Mean education: ", round(mean(education), 1), "\n")
cat("  Mean experience:", round(mean(experience), 1), "\n\n")

cat("Coefficients:\n")
print(round(coef(m_centered), 4))

cat("\nNow the 'education' coefficient (", round(coef(m_centered)["education_c"], 4),
    ") is the effect at AVERAGE experience.\n", sep = "")

# -----------------------------------------------------------------------------
# Part 5: Logarithmic Transformations
# -----------------------------------------------------------------------------

cat("\n=== Logarithmic Transformations ===\n\n")

# Generate right-skewed data
n <- 500
x <- runif(n, 1, 10)
y <- exp(2 + 0.3 * x + rnorm(n, sd = 0.5))

# Level-level
m_ll <- lm(y ~ x)

# Log-level
m_loglev <- lm(log(y) ~ x)

# Log-log
m_loglog <- lm(log(y) ~ log(x))

cat("Model comparisons:\n\n")

cat("1. Level-Level: Y ~ X\n")
cat("   Coefficient:", round(coef(m_ll)["x"], 2), "\n")
cat("   Interpretation: 1 unit increase in X -> ", round(coef(m_ll)["x"], 2),
    " unit increase in Y\n\n")

cat("2. Log-Level: log(Y) ~ X\n")
cat("   Coefficient:", round(coef(m_loglev)["x"], 4), "\n")
cat("   Interpretation: 1 unit increase in X -> ",
    round(100 * coef(m_loglev)["x"], 1), "% increase in Y\n\n")

cat("3. Log-Log: log(Y) ~ log(X)\n")
cat("   Coefficient:", round(coef(m_loglog)["log(x)"], 4), "\n")
cat("   Interpretation: 1% increase in X -> ",
    round(coef(m_loglog)["log(x)"], 2), "% increase in Y (elasticity)\n")

# -----------------------------------------------------------------------------
# Part 6: Polynomial Regression
# -----------------------------------------------------------------------------

cat("\n=== Polynomial Regression ===\n\n")

# Simulate age-wage relationship with diminishing returns
n <- 500
age <- runif(n, 18, 65)
wage <- 20 + 1.5 * age - 0.02 * age^2 + rnorm(n, sd = 5)

# Linear model
m_linear <- lm(wage ~ age)

# Quadratic model
m_quad <- lm(wage ~ age + I(age^2))

cat("Linear model: wage ~ age\n")
cat("  R-squared:", round(summary(m_linear)$r.squared, 4), "\n\n")

cat("Quadratic model: wage ~ age + age^2\n")
print(round(coef(m_quad), 4))
cat("  R-squared:", round(summary(m_quad)$r.squared, 4), "\n\n")

# Find the turning point
beta_1 <- coef(m_quad)["age"]
beta_2 <- coef(m_quad)["I(age^2)"]
turning_point <- -beta_1 / (2 * beta_2)

cat("Turning point (where wage peaks):\n")
cat("  X* = -beta_1 / (2 * beta_2) = ", round(turning_point, 1), " years old\n", sep = "")

# Marginal effect at different ages
cat("\nMarginal effect of age (= beta_1 + 2*beta_2*age):\n")
for (a in c(25, 35, 45, 55)) {
  effect <- beta_1 + 2 * beta_2 * a
  cat("  At age ", a, ": ", round(effect, 2),
      ifelse(effect > 0, " (increasing)", " (decreasing)"), "\n", sep = "")
}

# Visualize
par(mfrow = c(1, 2))

# Plot 1: Linear vs quadratic fit
plot(age, wage, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Linear vs Quadratic Fit",
     xlab = "Age", ylab = "Wage")
abline(m_linear, col = "blue", lwd = 2)
age_seq <- seq(18, 65, length.out = 100)
lines(age_seq, predict(m_quad, newdata = data.frame(age = age_seq)),
      col = "red", lwd = 2)
abline(v = turning_point, lty = 2, col = "gray")
legend("topright",
       legend = c("Linear", "Quadratic", "Turning point"),
       col = c("blue", "red", "gray"),
       lty = c(1, 1, 2), lwd = 2, bty = "n")

# Plot 2: Marginal effect of age
age_range <- seq(20, 60, by = 5)
marg_effects <- beta_1 + 2 * beta_2 * age_range
barplot(marg_effects, names.arg = age_range,
        col = ifelse(marg_effects > 0, "steelblue", "coral"),
        main = "Marginal Effect of Age",
        xlab = "Age", ylab = "Effect on Wage")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# Part 7: Summary Table
# -----------------------------------------------------------------------------

cat("\n=== Summary: Log Interpretations ===\n\n")

cat("Model            | Equation           | Interpretation of beta_1\n")
cat("-----------------+--------------------+--------------------------------\n")
cat("Level-Level      | Y = b0 + b1*X      | 1 unit X -> b1 units Y\n")
cat("Log-Level        | log(Y) = b0 + b1*X | 1 unit X -> 100*b1 % change Y\n")
cat("Level-Log        | Y = b0 + b1*log(X) | 1% X -> b1/100 units Y\n")
cat("Log-Log          | log(Y) = b0+b1*logX| 1% X -> b1 % Y (elasticity)\n")

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAYS ===\n")
cat("1. Interactions let the effect of X1 depend on X2\n")
cat("2. With interactions, 'main effect' is effect when other var = 0\n")
cat("3. Center variables to make main effects more interpretable\n")
cat("4. Log transforms give percentage interpretations\n")
cat("5. Polynomials capture curved relationships (diminishing returns)\n")
cat("6. All of these are still 'linear' regression (linear in parameters)\n")
