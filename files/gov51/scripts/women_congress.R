# ============================================================================
# Gov 51: Data Analysis and Politics
# Week 1 Thursday: Women in Congress
# ============================================================================
# This script accompanies the Week 1 Thursday lecture on coding workflows.
# We analyze the growth of women's representation in Congress since 1917.
#
# The data contains a planted error for teaching purposes.
# Your job: find the error using the validation techniques from class.
#
# To use this script:
#   - Run line by line (Cmd+Enter on Mac, Ctrl+Enter on Windows)
#   - Follow along with the lecture slides
# ============================================================================

# ----------------------------------------------------------------------------
# PART 1: THE QUESTION
# ----------------------------------------------------------------------------
# How has women's representation in Congress changed since Jeannette Rankin
# became the first woman elected to Congress in 1916?

# ----------------------------------------------------------------------------
# PART 2: LOAD THE DATA
# ----------------------------------------------------------------------------

# Load the data from the course website
# NOTE: This data contains a planted error for teaching purposes!
congress <- read.csv("https://scunning1975.github.io/files/gov51/data/women_congress_raw.csv")

# What did we get?
class(congress)
## [1] "data.frame"

# ----------------------------------------------------------------------------
# PART 3: THE EYEBALL TEST - First Look at the Data
# ----------------------------------------------------------------------------

# Step 1: Check dimensions
dim(congress)
## [1] 54  8
# 54 rows (Congresses from 1917-2024), 8 columns

# Step 2: Check column names
names(congress)
## [1] "year" "congress" "women_house" "women_senate"
## [5] "total_house" "total_senate" "party_control_house" "party_control_senate"

# Step 3: Look at the first few rows
head(congress)
##   year congress women_house women_senate total_house total_senate ...
## 1 1917       65           1            0         435           96 ...
## 2 1920       66           0            0         435           96 ...

# Step 4: Summary statistics
summary(congress)
# Look at min/max values - do they make sense?
# - year should be 1917-2024
# - women_house should be 0 to ~125 (growing over time)
# - women_senate should be 0 to ~26

# IMPORTANT: Do you see anything suspicious in the summary output?
# The max for women_house is 290 - that's impossible!
# (There are only 435 total House seats)

# ----------------------------------------------------------------------------
# PART 4: FINDING THE PROBLEM
# ----------------------------------------------------------------------------

# Now that we spotted something wrong, let's find it

# What rows have suspicious values?
# Since recent Congresses legitimately have >100 women, let's look for extreme values
which(congress$women_house > 200)
## [1] 38

# What year is that?
congress[38, ]
##    year congress women_house women_senate total_house total_senate ...
## 38 1992      102         290            2         435          100 ...

# The problem: 290 women in 1992 is clearly wrong
# It should be 29 (the "Year of the Woman" election)

# Also check for missing values
which(is.na(congress$women_senate))
## [1] 51

congress[51, ]
##    year congress women_house women_senate total_house total_senate ...
## 51 2018      115          84           NA         435          100 ...

# Found another issue: 2018 has a missing value for women_senate

# ----------------------------------------------------------------------------
# PART 5: COMPUTE PERCENTAGES
# ----------------------------------------------------------------------------

# Calculate the percentage of women in each chamber
congress$pct_women_house <- congress$women_house / congress$total_house * 100
congress$pct_women_senate <- congress$women_senate / congress$total_senate * 100

# Look at recent years
tail(congress[, c("year", "pct_women_house", "pct_women_senate")])
##    year pct_women_house pct_women_senate
## 49 2014            18.2             20.0
## 50 2016            19.3             20.0
## 51 2018            19.3               NA
## 52 2020            23.2             26.0
## 53 2022            27.1             24.0
## 54 2024            28.7             25.0

# ----------------------------------------------------------------------------
# PART 6: VISUALIZE THE TREND
# ----------------------------------------------------------------------------

# Base R plot (no packages needed)
plot(congress$year, congress$pct_women_house,
     type = "l",                      # line plot
     col = "blue",
     xlab = "Year",
     ylab = "Percent Women",
     main = "Women in the U.S. House of Representatives",
     ylim = c(0, 30))

# Add points to see individual data
points(congress$year, congress$pct_women_house, col = "blue", pch = 19, cex = 0.5)

# Notice the spike around 1992 - that's the data error!
# 290 women = 66% of the House, which is clearly wrong

# ----------------------------------------------------------------------------
# PART 7: USING GGPLOT2 (Optional)
# ----------------------------------------------------------------------------

# If you want fancier plots, use ggplot2
# install.packages("ggplot2")  # Run once if needed
library(ggplot2)

# Plot both chambers
ggplot(congress, aes(x = year)) +
  geom_line(aes(y = pct_women_house, color = "House")) +
  geom_line(aes(y = pct_women_senate, color = "Senate")) +
  geom_point(aes(y = pct_women_house, color = "House"), size = 1) +
  geom_point(aes(y = pct_women_senate, color = "Senate"), size = 1) +
  labs(x = "Year",
       y = "Percent Women",
       title = "Women in Congress, 1917-2024",
       color = "Chamber") +
  theme_minimal()

# The spike in the blue line shows our data error clearly!

# ----------------------------------------------------------------------------
# PART 8: KEY FINDINGS (After fixing the data)
# ----------------------------------------------------------------------------

# If we had the clean data, we'd find:
# - 1917: Jeannette Rankin - first woman in Congress (0.2% of House)
# - 1992: "Year of the Woman" - jumped from 6% to 11% in House
# - 2024: About 29% of House, 25% of Senate are women
# - Progress has been slow but accelerating since the 1990s

# ----------------------------------------------------------------------------
# SUMMARY: The Coding Workflow
# ----------------------------------------------------------------------------
# 1. Start with a question (How has women's representation changed?)
# 2. Load your data (read.csv)
# 3. Validate immediately (dim, names, head, summary)
# 4. Look for problems (impossible values, missing data)
# 5. Fix problems BEFORE analysis
# 6. Compute what you need
# 7. Visualize to understand patterns
# 8. Draw conclusions
#
# Always validate your data before trusting any results!
# ============================================================================
