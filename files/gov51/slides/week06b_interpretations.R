# week06b_interpretations.R
# Five model specifications showing different variable type combinations

library(wooldridge)
library(tidyverse)

data(wage1)

# Create derived variables
wage1 <- wage1 %>%
  mutate(
    lwage = log(wage),
    highwage = as.integer(wage > median(wage)),
    college = as.integer(educ >= 16)
  )

cat("=== Variable Summaries ===\n")
cat("Median wage:", median(wage1$wage), "\n")
cat("Share highwage:", mean(wage1$highwage), "\n")
cat("Share college:", mean(wage1$college), "\n\n")

# 1. Continuous X, Continuous Y
m1 <- lm(wage ~ educ, data = wage1)
cat("=== Model 1: wage ~ educ ===\n")
cat("  beta_educ:", round(coef(m1)["educ"], 4), "\n")
cat("  SE:", round(summary(m1)$coefficients["educ", 2], 4), "\n")
cat("  Interpretation: One more year of education -> +$",
    round(coef(m1)["educ"], 2), "/hr\n\n")

# 2. Continuous X, Log Y
m2 <- lm(lwage ~ educ, data = wage1)
cat("=== Model 2: log(wage) ~ educ ===\n")
cat("  beta_educ:", round(coef(m2)["educ"], 4), "\n")
cat("  SE:", round(summary(m2)$coefficients["educ", 2], 4), "\n")
cat("  Interpretation: One more year of education -> +",
    round(coef(m2)["educ"] * 100, 1), "% higher wages\n\n")

# 3. Binary X, Continuous Y (already have this from Model 1)
m3 <- lm(wage ~ female, data = wage1)
cat("=== Model 3: wage ~ female ===\n")
cat("  beta_female:", round(coef(m3)["female"], 4), "\n")
cat("  SE:", round(summary(m3)$coefficients["female", 2], 4), "\n")
cat("  Interpretation: Women earn $",
    round(abs(coef(m3)["female"]), 2), " less per hour\n\n")

# 4. Binary X, Log Y
m4 <- lm(lwage ~ female, data = wage1)
cat("=== Model 4: log(wage) ~ female ===\n")
cat("  beta_female:", round(coef(m4)["female"], 4), "\n")
cat("  SE:", round(summary(m4)$coefficients["female", 2], 4), "\n")
cat("  Interpretation: Women earn ~",
    round(abs(coef(m4)["female"]) * 100, 1), "% less\n")
cat("  Exact: ", round((exp(coef(m4)["female"]) - 1) * 100, 1), "%\n\n")

# 5. Binary X, Binary Y (LPM)
m5 <- lm(highwage ~ college, data = wage1)
cat("=== Model 5: highwage ~ college (LPM) ===\n")
cat("  beta_college:", round(coef(m5)["college"], 4), "\n")
cat("  SE:", round(summary(m5)$coefficients["college", 2], 4), "\n")
cat("  Interpretation: College grads are ",
    round(coef(m5)["college"] * 100, 1),
    " percentage points more likely to earn above median\n\n")

# Bonus: the full log model for the exercise
m_full <- lm(lwage ~ female + educ + exper + tenure, data = wage1)
cat("=== Full log model: log(wage) ~ female + educ + exper + tenure ===\n")
cat("Coefficients:\n")
print(round(summary(m_full)$coefficients, 4))
