# Week 2 Thursday: Descriptive Statistics & Data Visualization

**Date:** Thursday, February 6, 2026
**Slides:** `week02-thursday.pdf` (59 slides)
**R Script:** `descriptive_stats.R`
**Data:** `state_approval.csv`

---

## Where This Fits

### The Arc So Far

| When | Topic | Purpose |
|------|-------|---------|
| Week 1 Tue | Introduction | Why this course matters; what we'll learn |
| Week 1 Thu | R & Workflow | Directory structure, git, GitHub, reproducibility |
| **Week 2 Tue** | Data Visualization (George) | ggplot basics, grammar of graphics |
| **Week 2 Thu** | Descriptive Statistics | Summarizing data; telling stories with pictures |

### Connection to Problem Set 1

PS1 (due Wed Feb 11) asks students to:
- Download ACS data from IPUMS
- Create a summary statistics table (N, Mean, SD, Min, Max)
- Build histograms and interpret distributions
- Calculate weighted vs. unweighted means
- Explain what each number represents

Today's lecture gives them **every statistical concept and R function** they need:
- `mean()`, `median()`, `sd()`, `var()`, `min()`, `max()`
- `quantile()`, `weighted.mean()`, `summary()`
- `ggplot() + geom_histogram()`
- How to build and present a summary statistics table

---

## Thesis of Today's Lecture

**Numbers summarize. Visuals reveal. Use both.**

We want students to understand that:
1. Descriptive statistics compress information (mean, SD, percentiles)
2. Every compression loses something—know what you're hiding
3. Pictures reveal patterns that tables cannot show
4. The goal is **communication**: accurate work, clearly presented, respectful of the reader

---

## Structure of the Talk

### Part I: The Question (~5 min)
- Hook: "What does the 'typical' American think about the president?"
- Introduce the dataset: 50 states, presidential approval ratings
- Set up the problem: how do we summarize this?

### Part II: Measures of Center (~15 min)
- Mean: the balance point
- Median: the middle value
- When they differ: skewness
- Hand calculation moment (5 numbers) before jumping to code

### Part III: Measures of Spread (~15 min)
- Range, percentiles, quartiles
- Variance: why we square deviations
- Standard deviation: back to original units
- **Key insight**: Variance is always non-negative (from the formula)
- The n-1 question: degrees of freedom and bias

### Part IV: Weighted Statistics (~10 min)
- Not all observations count equally
- State populations → national average requires weighting
- Connection to PERWT in ACS data (PS1)

### Part V: Visualizing Distributions (~10 min)
- Histograms: what they show, how bin width matters
- Describing shape: symmetric, skewed, bimodal

### Part VI: Putting It Together (~5 min)
- The summary statistics table
- Code to build it (`data.frame` + `kable`)

### Part VII: Pictures Tell Stories (~20 min)
- **The rhetoric of quantitative work**: pictures → tables → words → truth
- **Visualization principles**: white space, dual y-axes dangers
- **Case study**: Craigslist personals research
  - Classification table (R/C/P/A)
  - The gender gap insight (5.7 - 2.1 = 3.6)
  - R/C ratio by age (the diverging figure)
  - Age preferences (men seek younger, women don't)
  - Sentiment analysis (faceted comparison)
  - **Narrative example**: The 40-year-old woman facing impossible choices
- **What made these pictures work**: one idea, facets, color, shading, white space, labels

---

## What We Want Them to Get Out of It

### Conceptual
1. Mean vs. median—and when the difference matters
2. Variance measures spread; SD puts it in interpretable units
3. Weighted statistics account for unequal representation
4. Histograms reveal distributional shape that summary stats hide

### Technical
1. Core R functions: `mean()`, `median()`, `sd()`, `var()`, `quantile()`, `weighted.mean()`
2. Building summary tables with `data.frame()` and `kable()`
3. Creating histograms with `ggplot() + geom_histogram()`

### Rhetorical
1. Tables are precise but dense; pictures reveal patterns
2. One idea per figure
3. Facets enable comparison; dual y-axes deceive
4. Interpret data with examples and narratives—make the reader *feel* the insight
5. Your figures should be readable without the surrounding text

---

## Notes for Delivery

### Don't Rush the Basics
They're Harvard students, but many haven't seen this before. The hand calculation of mean (5 numbers) builds intuition before `mean()` becomes a black box.

### The n-1 Explanation
Keep it intuitive: "We estimated the mean first, so deviations from x-bar are artificially small. Dividing by n-1 corrects for this bias." Don't derive it—just give them the intuition and tell them R does the right thing by default.

### The Craigslist Case Study
This is the payoff. The early slides teach tools; the case study shows why those tools matter. The 40-year-old woman slide is the emotional core—it takes abstract numbers and makes them human.

### Time Management
- Parts I-VI: ~60 minutes (the "textbook" material)
- Part VII: ~15 minutes (the "why it matters" material)
- Leave 5 minutes for questions

If running short, you can trim the sentiment slides. Don't trim the gender gap insight or the 40-year-old woman—those are the thesis in action.

---

## Files in This Directory

| File | Purpose |
|------|---------|
| `week02-thursday.tex` | Beamer source |
| `week02-thursday.pdf` | Compiled slides |
| `week02-thursday-notes.md` | This document |
| `week02_plan.md` | Original planning notes |
| `descriptive_stats.R` | R script for students |
| `state_approval.csv` | Dataset for lecture |
| `rc_by_age.png` | R/C ratio figure |
| `age_prefs_gap.png` | Age preferences figure |
| `sentiment_romantic.png` | Sentiment (romantic) figure |
| `sentiment_casual.png` | Sentiment (casual) figure |

---

## Post-Lecture

After class:
1. Slides are already live on website
2. R script is already live on website
3. Students can access data via URL in the script
4. Section this week: IPUMS setup help, PS1 questions
