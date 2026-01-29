# Gov 51 Datasets

This folder contains datasets used in Gov 51: Data Analysis and Politics at Harvard University.

## Available Datasets

### Women in Congress
- **Files**: `women_congress.csv`, `women_congress_raw.csv`
- **Source**: Center for American Women and Politics (CAWP), Rutgers University
- **URL**: https://cawp.rutgers.edu/data/levels-office/congress
- **Description**: Historical counts of women serving in the U.S. House and Senate by Congress (1917-2024)
- **Variables**:
  - `year`: Election year
  - `congress`: Congress number (65th-118th)
  - `women_house`: Number of women in the House
  - `women_senate`: Number of women in the Senate
  - `total_house`: Total House seats (435)
  - `total_senate`: Total Senate seats (96 until 1960, then 100)
  - `party_control_house`: Majority party in House (D/R)
  - `party_control_senate`: Majority party in Senate (D/R)
- **Note**: `women_congress_raw.csv` contains intentional errors for teaching data validation:
  - 1992: `women_house = 290` (should be 29)
  - 2018: `women_senate = NA` (missing value)

### Voter Turnout (Coming Soon)
- **Source**: QSS textbook / U.S. Elections Project
- **Description**: Voter turnout rates using VAP and VEP denominators

---

## How to Load These Datasets in R

```r
# Load directly from GitHub
url <- "https://raw.githubusercontent.com/scunning1975/scunning1975.github.io/master/files/gov51/data/women_congress.csv"
congress <- read.csv(url)

# Or download and load locally
congress <- read.csv("data/women_congress.csv")
```

## Data Use Policy

These datasets are provided for educational purposes. Please cite original sources when using in research or publications.

---
*Last updated: January 2026*
