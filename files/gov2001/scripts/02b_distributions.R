# =============================================================================
# Gov 2001: Lecture 02b - Famous Distributions
# Spring 2026
# =============================================================================

# This script demonstrates how to work with probability distributions in R.
# Key functions: d (density), p (CDF), q (quantile), r (random samples)

# -----------------------------------------------------------------------------
# The Four Functions: d, p, q, r
# -----------------------------------------------------------------------------

# Every distribution in R has four functions:
# - d: density (PDF/PMF value at x)
# - p: probability (CDF: P(X <= x))
# - q: quantile (inverse CDF)
# - r: random samples

# Example with Normal(0,1):
dnorm(0)        # d = density (PDF value at x=0) -> 0.3989
pnorm(1.96)     # p = probability (CDF: P(X <= 1.96)) -> 0.975
qnorm(0.975)    # q = quantile (inverse CDF) -> 1.96
rnorm(10)       # r = 10 random samples

# -----------------------------------------------------------------------------
# Discrete Distributions
# -----------------------------------------------------------------------------

# --- Bernoulli (single coin flip) ---
# Bernoulli is Binomial with size=1
rbinom(10, size = 1, prob = 0.5)  # 10 coin flips
mean(rbinom(10000, size = 1, prob = 0.3))  # Should be close to 0.3

# --- Binomial (number of successes in n trials) ---
# X ~ Binomial(n, p): number of successes in n independent Bernoulli trials
dbinom(3, size = 10, prob = 0.5)  # P(X = 3) when n=10, p=0.5
pbinom(3, size = 10, prob = 0.5)  # P(X <= 3)
rbinom(100, size = 10, prob = 0.5)  # 100 draws

# Visualize Binomial PMF
x <- 0:10
plot(x, dbinom(x, size = 10, prob = 0.5), type = "h", lwd = 2,
     main = "Binomial(10, 0.5)", xlab = "k", ylab = "P(X = k)")

# --- Poisson (count of rare events) ---
# X ~ Poisson(lambda): counts with rate lambda
dpois(3, lambda = 2)  # P(X = 3) when lambda = 2
rpois(100, lambda = 5)  # 100 draws with lambda = 5

# Visualize Poisson PMF
x <- 0:15
plot(x, dpois(x, lambda = 5), type = "h", lwd = 2,
     main = "Poisson(5)", xlab = "k", ylab = "P(X = k)")

# -----------------------------------------------------------------------------
# Continuous Distributions
# -----------------------------------------------------------------------------

# --- Uniform (equally likely over interval) ---
# X ~ Uniform(a, b): constant density over [a, b]
dunif(0.5, min = 0, max = 1)  # Density at 0.5
punif(0.75, min = 0, max = 1)  # P(X <= 0.75) = 0.75
runif(100, min = 0, max = 1)  # 100 draws from U(0,1)

# --- Normal (Gaussian, bell curve) ---
# X ~ Normal(mu, sigma): the famous bell curve
dnorm(0, mean = 0, sd = 1)  # Standard normal density at 0
pnorm(0, mean = 0, sd = 1)  # P(X <= 0) = 0.5
qnorm(0.975, mean = 0, sd = 1)  # 97.5th percentile -> 1.96
rnorm(100, mean = 0, sd = 1)  # 100 standard normal draws

# The 68-95-99.7 rule
pnorm(1) - pnorm(-1)    # ~68% within 1 SD
pnorm(2) - pnorm(-2)    # ~95% within 2 SD
pnorm(3) - pnorm(-3)    # ~99.7% within 3 SD

# --- Exponential (waiting times) ---
# X ~ Exponential(rate): time until first event
dexp(1, rate = 2)  # Density at x=1 with rate=2
pexp(1, rate = 2)  # P(X <= 1)
rexp(100, rate = 2)  # 100 draws

# -----------------------------------------------------------------------------
# Visualizing Distributions
# -----------------------------------------------------------------------------

# Set up 2x2 plot
par(mfrow = c(2, 2))

# Binomial
x_binom <- 0:20
plot(x_binom, dbinom(x_binom, 20, 0.5), type = "h", lwd = 2,
     main = "Binomial(20, 0.5)", xlab = "x", ylab = "P(X = x)",
     col = "steelblue")

# Poisson
x_pois <- 0:20
plot(x_pois, dpois(x_pois, lambda = 7), type = "h", lwd = 2,
     main = "Poisson(7)", xlab = "x", ylab = "P(X = x)",
     col = "forestgreen")

# Normal
x_norm <- seq(-4, 4, length.out = 200)
plot(x_norm, dnorm(x_norm), type = "l", lwd = 2,
     main = "Standard Normal", xlab = "x", ylab = "Density",
     col = "darkred")

# Exponential
x_exp <- seq(0, 5, length.out = 200)
plot(x_exp, dexp(x_exp, rate = 1), type = "l", lwd = 2,
     main = "Exponential(1)", xlab = "x", ylab = "Density",
     col = "purple")

par(mfrow = c(1, 1))  # Reset

# -----------------------------------------------------------------------------
# Sampling and the Law of Large Numbers (preview)
# -----------------------------------------------------------------------------

# As we take more samples, the sample mean approaches the true mean
set.seed(42)

n_samples <- c(10, 100, 1000, 10000)
true_mean <- 5  # Poisson(5) has mean = 5

cat("Sample means from Poisson(5):\n")
for (n in n_samples) {
  sample_mean <- mean(rpois(n, lambda = 5))
  cat(sprintf("  n = %5d: sample mean = %.3f\n", n, sample_mean))
}
cat(sprintf("  True mean: %.3f\n", true_mean))
