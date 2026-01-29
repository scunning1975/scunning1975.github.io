# ============================================================================
# Gov 51: Data Analysis and Politics
# Week 1: Introduction
# ============================================================================
# This script accompanies the Week 1 lecture slides.
# It covers: R basics, vectors, functions, and analyzing voter turnout data.
#
# To use this script:
#   - Run line by line (Cmd+Enter on Mac, Ctrl+Enter on Windows)
#   - Or source the entire file to see all output
#   - Make sure ggplot2 is installed (see Part 6)
# ============================================================================

# ----------------------------------------------------------------------------
# PART 1: R AS A CALCULATOR
# ----------------------------------------------------------------------------

# Basic arithmetic operations
5 + 3       # Addition
## [1] 8

5 - 3       # Subtraction
## [1] 2

5 * 3       # Multiplication
## [1] 15

5 / 3       # Division
## [1] 1.666667

5 ^ 3       # Exponentiation
## [1] 125

sqrt(16)    # Square root
## [1] 4

# ----------------------------------------------------------------------------
# PART 2: OBJECTS
# ----------------------------------------------------------------------------

# Store results with the assignment operator <-
result <- 5 + 3
result
## [1] 8

# Objects can hold text (strings) too
my_name <- "Scott"
my_name
## [1] "Scott"

# Use meaningful names!
voter_turnout <- 0.62    # Good: descriptive
x <- 0.62                # Bad: what does x mean?

# ----------------------------------------------------------------------------
# PART 3: VECTORS
# ----------------------------------------------------------------------------

# A vector is a collection of values of the same type
# Election years in our data
years <- c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008)
years

# Access elements
years[1]        # First element: 1980
## [1] 1980

years[c(1,3)]   # First and third: 1980, 1988
## [1] 1980 1988

years[-1]       # All except first
## [1] 1984 1988 1992 1996 2000 2004 2008

# ----------------------------------------------------------------------------
# PART 4: VECTOR OPERATIONS
# ----------------------------------------------------------------------------

# Operations apply to *every element*

# Total votes cast (thousands) in presidential elections
total_votes <- c(86515, 92653, 91595, 104405,
                 96263, 105375, 122295, 131304)

# Convert to millions
total_votes / 1000
## [1] 86.5 92.7 91.6 104.4 96.3 105.4 122.3 131.3

# Growth relative to 1980
total_votes / total_votes[1]
## [1] 1.00 1.07 1.06 1.21 1.11 1.22 1.41 1.52

# ----------------------------------------------------------------------------
# PART 5: FUNCTIONS
# ----------------------------------------------------------------------------

# Functions take inputs and produce outputs
length(total_votes)  # Number of elections
## [1] 8

mean(total_votes)    # Average votes (thousands)
## [1] 103801

min(total_votes)     # Minimum
## [1] 86515

# ----------------------------------------------------------------------------
# PART 6: LOADING AND EXAMINING DATA
# ----------------------------------------------------------------------------

# Install ggplot2 if you haven't already (uncomment and run once)
# install.packages("ggplot2")

# Load the turnout data
# The slides show: turnout <- read.csv("turnout.csv")
# We use a URL so it works without downloading the file first:
turnout <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/turnout.csv")

# What do we have?
dim(turnout)
## [1] 14  9

names(turnout)
## [1] "year" "VEP" "VAP" "total" "ANES"
## [6] "felons" "noncit" "overseas" "osvoters"

# First few rows
head(turnout, 3)
##   year    VEP    VAP total ANES felons noncit overseas osvoters
## 1 1980 159635 164445 86515   71    802   5756     1803       NA
## 2 1982 160467 166028 67616   60    960   6641     1982       NA
## 3 1984 167702 173995 92653   74   1165   7482     2361       NA

# The data contains 14 elections (1980-2008), including midterms
# VEP, VAP, total: in thousands
# ANES: self-reported turnout (%)
# Notice 1982 has lower total -- midterm election

# ----------------------------------------------------------------------------
# PART 7: ACCESSING COLUMNS
# ----------------------------------------------------------------------------

# Use $ to extract a column
turnout$year
## [1] 1980 1982 1984 1986 1988 1990 1992 1994
## [9] 1996 1998 2000 2002 2004 2008

turnout$total
## [1]  86515  67616  92653  64991  91595  67859
## [7] 104405  75106  96263  72537 105375  78382 ...

# Each column is a vector

# ----------------------------------------------------------------------------
# PART 8: COMPUTING TURNOUT RATES
# ----------------------------------------------------------------------------

# VAP turnout = total votes / (VAP + overseas) * 100
VAP_turnout <- turnout$total /
               (turnout$VAP + turnout$overseas) * 100

VAP_turnout
## [1] 52.0 40.6 52.9 36.4 50.0 36.3 54.4 38.3
## [9] 47.5 35.2 49.7 36.2 55.2 55.7

# VEP turnout = total votes / VEP * 100
VEP_turnout <- turnout$total / turnout$VEP * 100

VEP_turnout
## [1] 54.2 42.1 55.2 38.1 52.8 38.4 58.1 41.1
## [9] 51.7 38.1 54.2 39.5 60.1 61.6

# Presidential years: around 50-60%. Midterms: around 37-42%.
# VEP turnout is higher -- because denominator is smaller.

# ----------------------------------------------------------------------------
# PART 9: COMPARING VEP AND VAP TURNOUT
# ----------------------------------------------------------------------------

# How different are they?
VEP_turnout - VAP_turnout
## [1] 2.2 1.5 2.3 1.7 2.8 2.1 3.7 2.8
## [9] 4.2 2.9 4.5 3.3 4.9 5.9

mean(VEP_turnout - VAP_turnout)
## [1] 3.2

# On average, VAP understates turnout by 3.2 percentage points.
# The gap is *growing* over time as the ineligible population grows.

# ----------------------------------------------------------------------------
# PART 10: SOCIAL DESIRABILITY BIAS
# ----------------------------------------------------------------------------

# The ANES survey asks people if they voted.
# Compare self-reported (ANES) to actual (VEP)
turnout$ANES - VEP_turnout
## [1] 16.8 17.9 18.8 14.9 17.2  8.6 16.9 14.9
## [9] 21.3 13.9 18.8 22.5 16.9 16.4

mean(turnout$ANES - VEP_turnout)
## [1] 16.8

# People overreport voting by about 17 percentage points!
# This is called "social desirability bias."

# ----------------------------------------------------------------------------
# PART 11: VISUALIZING DATA WITH GGPLOT2
# ----------------------------------------------------------------------------

library(ggplot2)

# The grammar of graphics builds plots in layers:
#   ggplot(data, aes(x = ..., y = ...)) +
#     geom_*()
#
# - ggplot(): Initialize with data
# - aes(): Map variables to aesthetics (x, y, color, etc.)
# - geom_*(): Add geometric objects (points, lines, bars)

# Add turnout rates to our data frame
turnout$VEP_turnout <- turnout$total / turnout$VEP * 100
turnout$VAP_turnout <- turnout$total / (turnout$VAP + turnout$overseas) * 100

# Plot VEP turnout over time
ggplot(turnout, aes(x = year, y = VEP_turnout)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Turnout (%)",
       title = "Voter Turnout in US Elections")

# Comparing VEP vs VAP turnout
ggplot(turnout, aes(x = year)) +
  geom_line(aes(y = VEP_turnout, color = "VEP")) +
  geom_line(aes(y = VAP_turnout, color = "VAP")) +
  labs(x = "Year", y = "Turnout (%)",
       title = "VEP vs VAP Turnout Rates",
       color = "Measure")

# ----------------------------------------------------------------------------
# SUMMARY: What We Learned
# ----------------------------------------------------------------------------
# 1. Measurement matters: VAP vs VEP gives different answers
# 2. Self-reports are biased: People overreport socially desirable behavior
# 3. The gap is growing: As ineligible population increases, VAP becomes
#    more misleading
#
# This is what quantitative social science looks like:
#   - Start with a question
#   - Get data
#   - Compute and compare
#   - Draw conclusions
# ============================================================================
