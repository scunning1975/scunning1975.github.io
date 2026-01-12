# =============================================================================
# Gov 2001: Lecture 12b - Clustered Standard Errors
# Spring 2026
# =============================================================================

# This script demonstrates why clustering matters and how to implement it.
# Key insight: Ignoring clustering leads to SEs that are too small.

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

set.seed(2001)

# Install if needed:
# install.packages("estimatr")

library(estimatr)

# -----------------------------------------------------------------------------
# Part 1: Simulating Clustered Data
# -----------------------------------------------------------------------------

cat("=== Simulating Clustered Data ===\n\n")

# Parameters
n_clusters <- 50
n_per_cluster <- 20
n <- n_clusters * n_per_cluster

# Create cluster identifier
cluster <- rep(1:n_clusters, each = n_per_cluster)

# Cluster-level effects (unobserved factors affecting all cluster members)
cluster_effect <- rnorm(n_clusters, mean = 0, sd = 2)
cluster_effect_expanded <- cluster_effect[cluster]

# Individual-level error
indiv_error <- rnorm(n, mean = 0, sd = 1)

# Total error = cluster effect + individual effect
total_error <- cluster_effect_expanded + indiv_error

# Treatment (randomized at individual level)
treatment <- rbinom(n, 1, 0.5)

# True effect of treatment is 2
true_effect <- 2
y <- 5 + true_effect * treatment + total_error

data <- data.frame(y = y, treatment = treatment, cluster = cluster)

cat("Data structure:\n")
cat("  Number of clusters:        ", n_clusters, "\n")
cat("  Observations per cluster:  ", n_per_cluster, "\n")
cat("  Total observations:        ", n, "\n")
cat("  True treatment effect:     ", true_effect, "\n\n")

# Within-cluster correlation (ICC)
var_cluster <- var(cluster_effect)
var_total <- var_cluster + 1  # cluster variance + individual variance
icc <- var_cluster / var_total

cat("Intraclass Correlation (ICC):\n")
cat("  ICC = ", round(icc, 3), "\n")
cat("  This means", round(100 * icc), "% of variance is at cluster level\n")

# -----------------------------------------------------------------------------
# Part 2: Comparing Standard vs Clustered SEs
# -----------------------------------------------------------------------------

cat("\n=== Comparing Standard vs Clustered SEs ===\n\n")

# Standard OLS (ignoring clustering)
m_ols <- lm(y ~ treatment, data = data)

# OLS with robust SEs (still ignoring clustering)
m_robust <- lm_robust(y ~ treatment, data = data, se_type = "HC2")

# Cluster-robust SEs
m_clustered <- lm_robust(y ~ treatment, data = data, clusters = cluster)

# Extract results
se_ols <- summary(m_ols)$coef["treatment", "Std. Error"]
se_robust <- m_robust$std.error["treatment"]
se_clustered <- m_clustered$std.error["treatment"]

cat("Standard Errors for Treatment Coefficient:\n\n")
cat("Method              SE        t-stat    p-value\n")
cat("----------------- -------- --------- ----------\n")
cat("Classical:        ", sprintf("%.4f", se_ols),
    "   ", sprintf("%.2f", coef(m_ols)["treatment"] / se_ols),
    "     ", sprintf("%.4f", 2 * pt(-abs(coef(m_ols)["treatment"] / se_ols), df = n - 2)), "\n")
cat("Robust (HC2):     ", sprintf("%.4f", se_robust),
    "   ", sprintf("%.2f", coef(m_ols)["treatment"] / se_robust),
    "     ", sprintf("%.4f", 2 * pt(-abs(coef(m_ols)["treatment"] / se_robust), df = n - 2)), "\n")
cat("Clustered:        ", sprintf("%.4f", se_clustered),
    "   ", sprintf("%.2f", coef(m_ols)["treatment"] / se_clustered),
    "     ", sprintf("%.4f", 2 * pt(-abs(coef(m_ols)["treatment"] / se_clustered), df = n_clusters - 1)), "\n")

cat("\nNote: Clustered SE is much LARGER than standard or robust SE.\n")
cat("      This reflects the reduced 'effective sample size'.\n")

# -----------------------------------------------------------------------------
# Part 3: Simulation - Why Standard SEs Are Wrong
# -----------------------------------------------------------------------------

cat("\n=== Simulation: Coverage Rates ===\n\n")

n_sims <- 1000
true_effect <- 2

# Store results
reject_ols <- 0
reject_robust <- 0
reject_clustered <- 0

for (i in 1:n_sims) {
  # Generate clustered data
  cluster_eff <- rnorm(n_clusters, mean = 0, sd = 2)
  cluster_eff_exp <- cluster_eff[cluster]
  indiv_err <- rnorm(n, mean = 0, sd = 1)
  treat <- rbinom(n, 1, 0.5)
  y_sim <- 5 + true_effect * treat + cluster_eff_exp + indiv_err

  # Fit models
  m1 <- lm(y_sim ~ treat)
  m2 <- lm_robust(y_sim ~ treat, se_type = "HC2")
  m3 <- lm_robust(y_sim ~ treat, clusters = cluster)

  # Test H0: beta = 2 (true value) at alpha = 0.05
  # Actually, let's test H0: beta = 0 to see rejection rates
  # Under H0: beta = true_effect, we want ~5% rejection

  # Using CI approach: does CI contain true value?
  ci_ols <- coef(m1)["treat"] + c(-1.96, 1.96) * summary(m1)$coef["treat", 2]
  ci_robust <- coef(m1)["treat"] + c(-1.96, 1.96) * m2$std.error["treat"]
  ci_clustered <- coef(m1)["treat"] + c(-1.96, 1.96) * m3$std.error["treat"]

  if (ci_ols[1] > true_effect | ci_ols[2] < true_effect) reject_ols <- reject_ols + 1
  if (ci_robust[1] > true_effect | ci_robust[2] < true_effect) reject_robust <- reject_robust + 1
  if (ci_clustered[1] > true_effect | ci_clustered[2] < true_effect) reject_clustered <- reject_clustered + 1
}

cat("Rejection Rates of True H0 (should be ~5%):\n")
cat("  Classical SEs:   ", round(100 * reject_ols / n_sims, 1), "%\n")
cat("  Robust SEs:      ", round(100 * reject_robust / n_sims, 1), "%\n")
cat("  Clustered SEs:   ", round(100 * reject_clustered / n_sims, 1), "%\n")

cat("\nInterpretation:\n")
cat("  Standard and robust SEs OVER-reject because they're too small.\n")
cat("  Only clustered SEs give proper coverage.\n")

# -----------------------------------------------------------------------------
# Part 4: Visualizing Within-Cluster Correlation
# -----------------------------------------------------------------------------

cat("\n=== Visualizing Clustered Data ===\n")

par(mfrow = c(1, 2))

# Plot 1: Show observations colored by cluster
# Pick first 10 clusters for visibility
subset_clusters <- 1:10
subset_idx <- cluster %in% subset_clusters

plot(jitter(treatment[subset_idx], amount = 0.1),
     y[subset_idx],
     col = cluster[subset_idx], pch = 16,
     main = "Y by Treatment (colored by cluster)",
     xlab = "Treatment", ylab = "Y",
     xaxt = "n")
axis(1, at = c(0, 1), labels = c("Control", "Treatment"))
legend("topleft", legend = paste("Cluster", 1:10),
       col = 1:10, pch = 16, ncol = 2, cex = 0.7)

# Plot 2: Cluster means
cluster_means <- aggregate(y ~ cluster, data = data, mean)
plot(cluster_means$cluster, cluster_means$y, pch = 16,
     main = "Cluster Means of Y",
     xlab = "Cluster ID", ylab = "Mean Y")
abline(h = mean(y), col = "red", lty = 2)

par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------
# Part 5: Different Levels of Clustering
# -----------------------------------------------------------------------------

cat("\n=== Effect of ICC on Standard Error Inflation ===\n\n")

# Show how different ICC levels affect the problem
icc_levels <- c(0, 0.1, 0.3, 0.5, 0.7)

cat("ICC    SE_ols    SE_clustered   Ratio\n")
cat("-----  --------  ------------   -----\n")

for (icc_val in icc_levels) {
  # Calculate cluster and individual SD to get target ICC
  # ICC = var_cluster / (var_cluster + var_indiv)
  # If var_indiv = 1, then var_cluster = ICC / (1 - ICC)
  if (icc_val == 0) {
    var_c <- 0
  } else {
    var_c <- icc_val / (1 - icc_val)
  }
  sd_cluster <- sqrt(var_c)

  # Generate data
  c_eff <- rnorm(n_clusters, sd = sd_cluster)[cluster]
  i_err <- rnorm(n)
  y_temp <- 5 + 2 * treatment + c_eff + i_err

  # Fit models
  m_temp_ols <- lm(y_temp ~ treatment)
  m_temp_clust <- lm_robust(y_temp ~ treatment, clusters = cluster)

  se_temp_ols <- summary(m_temp_ols)$coef["treatment", 2]
  se_temp_clust <- m_temp_clust$std.error["treatment"]

  cat(sprintf("%.1f    %.4f    %.4f         %.1f\n",
              icc_val, se_temp_ols, se_temp_clust, se_temp_clust / se_temp_ols))
}

cat("\nAs ICC increases, clustered SEs get much larger than standard SEs.\n")

# -----------------------------------------------------------------------------
# Part 6: Number of Clusters Matters
# -----------------------------------------------------------------------------

cat("\n=== Number of Clusters Matters ===\n\n")

cat("Rule of thumb for cluster-robust SEs:\n")
cat("  G >= 50:      Generally reliable\n")
cat("  20 <= G < 50: Use with caution\n")
cat("  G < 20:       May be unreliable; consider alternatives\n\n")

cat("Our simulation: G =", n_clusters, "clusters\n")
cat("This is at the borderline where cluster-robust SEs work well.\n")

# -----------------------------------------------------------------------------
# Part 7: Treatment Assigned at Cluster Level
# -----------------------------------------------------------------------------

cat("\n=== Example: Treatment Assigned at Cluster Level ===\n\n")

# Now treatment varies only at cluster level (cluster-randomized experiment)
treat_cluster <- rbinom(n_clusters, 1, 0.5)
treat_expanded <- treat_cluster[cluster]

# Generate outcome
y_cluster_treat <- 5 + 2 * treat_expanded + cluster_effect_expanded + indiv_error

# Fit models
m_ct_ols <- lm(y_cluster_treat ~ treat_expanded)
m_ct_clustered <- lm_robust(y_cluster_treat ~ treat_expanded, clusters = cluster)

se_ct_ols <- summary(m_ct_ols)$coef[2, 2]
se_ct_clust <- m_ct_clustered$std.error[2]

cat("When treatment is assigned at cluster level:\n\n")
cat("  Standard SE:    ", round(se_ct_ols, 4), "\n")
cat("  Clustered SE:   ", round(se_ct_clust, 4), "\n")
cat("  Ratio:          ", round(se_ct_clust / se_ct_ols, 1), "x\n\n")

cat("The inflation is even MORE severe when treatment is at cluster level!\n")
cat("This is because variation in treatment only comes from", n_clusters, "clusters.\n")

# -----------------------------------------------------------------------------
# Part 8: Practical Implementation
# -----------------------------------------------------------------------------

cat("\n=== Practical Implementation ===\n\n")

cat("In R (estimatr package):\n")
cat("  lm_robust(y ~ x, data = df, clusters = cluster_var)\n\n")

cat("In R (sandwich package):\n")
cat("  m <- lm(y ~ x, data = df)\n")
cat("  coeftest(m, vcov = vcovCL(m, cluster = ~cluster_var))\n\n")

cat("In Stata:\n")
cat("  reg y x, cluster(cluster_var)\n")

# -----------------------------------------------------------------------------
# Key Takeaways
# -----------------------------------------------------------------------------

cat("\n=== KEY TAKEAWAYS ===\n")
cat("1. Errors are often correlated within groups (clusters)\n")
cat("2. Ignoring clustering makes standard errors TOO SMALL\n")
cat("3. This leads to over-rejection of true null hypotheses\n")
cat("4. Cluster at the level of sampling or treatment assignment\n")
cat("5. Need enough clusters (~50+) for cluster-robust SEs to work\n")
cat("6. Fixed effects do NOT eliminate the need to cluster\n")
