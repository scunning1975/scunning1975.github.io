# ============================================================================
# Gov 51: Data Analysis and Politics
# Week 1 Thursday: Women in Congress
# ============================================================================
# This script accompanies the Week 1 Thursday lecture.
# Follow along with the slides - run each section as we go through it in class.
#
# To use this script:
#   - Run line by line (Cmd+Enter on Mac, Ctrl+Enter on Windows)
#   - Follow along with the lecture slides
# ============================================================================

# ----------------------------------------------------------------------------
# THE QUESTION
# ----------------------------------------------------------------------------
# How has women's representation in Congress changed since Jeannette Rankin
# became the first woman elected to Congress in 1916?

# ----------------------------------------------------------------------------
# LOADING THE DATA
# ----------------------------------------------------------------------------

# Load data directly from the web
congress <- read.csv("https://scunning1975.github.io/files/gov51/data/women_congress_raw.csv")

# What do we have?
dim(congress)
## [1] 54  8
# 54 Congresses (1917-2024), 8 variables

names(congress)
## [1] "year" "congress" "women_house" "women_senate"
## [5] "total_house" "total_senate" ...

# ----------------------------------------------------------------------------
# FIRST CHECK: WHAT DOES IT LOOK LIKE?
# ----------------------------------------------------------------------------

head(congress, 4)
##   year congress women_house women_senate total_house
## 1 1917       65           1            0         435
## 2 1920       66           0            0         435
## 3 1922       67           1            1         435
## 4 1924       68           1            0         435

# Quick sanity checks:
# - Years look right (1917 onwards)
# - Small numbers of women early on (makes historical sense)
# - Total House seats = 435

# ----------------------------------------------------------------------------
# COMPUTING REPRESENTATION
# ----------------------------------------------------------------------------

# Let's calculate the percentage of women in the House
congress$pct_women <- congress$women_house / congress$total_house * 100

# Quick check
range(congress$pct_women)
## [1]  0.00000 66.66667

# Hmm, 67% seems high. Let's visualize it to see the trend...

# ----------------------------------------------------------------------------
# LET'S PLOT IT
# ----------------------------------------------------------------------------

library(ggplot2)

ggplot(congress, aes(x = year, y = pct_women)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(x = "Year", y = "Women (%)",
       title = "Women in the U.S. House")

# WAIT... WHAT'S THAT SPIKE?!

# ----------------------------------------------------------------------------
# FINDING THE PROBLEM
# ----------------------------------------------------------------------------

# The visualization showed us something is wrong. Now let's find it:

# Which row has the maximum?
which.max(congress$pct_women)
## [1] 38

# Look at that row
congress[38, c("year", "women_house", "total_house")]
##    year women_house total_house
## 38 1992         290         435

# 290 women in the House in 1992?
# That's IMPOSSIBLE - there are only 435 seats total!

# Data entry error: Someone typed 290 instead of 29.

# ----------------------------------------------------------------------------
# FIXING THE ERROR
# ----------------------------------------------------------------------------

# Fix the typo
congress$women_house[38] <- 29

# Recalculate
congress$pct_women <- congress$women_house / congress$total_house * 100

# Check again
range(congress$pct_women)
## [1]  0.000000 28.735632

# Now the max is ~29% - that's plausible!

# ----------------------------------------------------------------------------
# THE REVEAL
# ----------------------------------------------------------------------------
# I planted that error in the data.
#
# Why? Because this happens ALL THE TIME with real data:
#   - Typos in data entry (290 vs 29)
#   - Units that don't match (thousands vs. actual)
#   - Missing values coded as 999 or -1
#   - Copy-paste errors, OCR mistakes, merging gone wrong
#
# Without visualizing first, you might never notice.
# The eyeball test catches errors that code alone cannot.

# ----------------------------------------------------------------------------
# NOW WE CAN TRUST OUR DATA
# ----------------------------------------------------------------------------

# Plot the corrected data
ggplot(congress, aes(x = year, y = pct_women)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(x = "Year", y = "Women (%)",
       title = "Women in the U.S. House of Representatives")

# Slow growth for decades, then acceleration starting in the 1990s.

# ----------------------------------------------------------------------------
# COMPARING HOUSE AND SENATE
# ----------------------------------------------------------------------------

# Add Senate percentage
congress$pct_senate <- congress$women_senate / congress$total_senate * 100

# Plot both chambers
ggplot(congress, aes(x = year)) +
  geom_line(aes(y = pct_women, color = "House"), linewidth = 1) +
  geom_line(aes(y = pct_senate, color = "Senate"), linewidth = 1) +
  geom_point(aes(y = pct_women, color = "House")) +
  geom_point(aes(y = pct_senate, color = "Senate")) +
  labs(x = "Year", y = "Women (%)",
       title = "Women in Congress by Chamber",
       color = "Chamber") +
  theme_minimal()

# The Senate lagged behind the House until recently.

# ----------------------------------------------------------------------------
# SAVING YOUR FIGURE
# ----------------------------------------------------------------------------

# Don't screenshot. Save with code:

# Create the plot
women_plot <- ggplot(congress, aes(x = year, y = pct_women)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(x = "Year", y = "Women (%)",
       title = "Women in the House")

# Save it
ggsave("women_congress.png", plot = women_plot, width = 8, height = 5)

# Now when you update your analysis, just re-run the script.

# ----------------------------------------------------------------------------
# SUMMARY: The Coding Workflow
# ----------------------------------------------------------------------------
# 1. Start with a question
# 2. Load your data
# 3. Quick checks (dim, names, head)
# 4. Compute what you need
# 5. VISUALIZE - this is where you catch errors!
# 6. Investigate problems
# 7. Fix and verify
# 8. Proceed with analysis
#
# Always validate your data before trusting any results!
# ============================================================================
