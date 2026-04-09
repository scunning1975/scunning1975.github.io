# Website Consistency Audit

**Date:** 2026-03-18
**Auditor:** Claude (automated structural audit)
**Scope:** All primary HTML pages in scunning1975.github.io
**Reference template:** index.html (homepage)

---

## Reference Template (index.html)

The homepage establishes the canonical structure that all other pages should follow:

- **CSS:** `css/bootstrap.css` + `css/scunning.css`
- **Fonts:** Google Fonts — Merriweather, Source Sans Pro, Muli
- **Icons:** Font Awesome kit (`kit.fontawesome.com/e7b3ba2953.js`)
- **Analytics:** Google Analytics (UA-4894172-2)
- **Header name:** `scott cunningham` (lowercase)
- **Header title:** `ben h. williams professor of economics` / `baylor university`
- **Nav structure:** `<header class="container app-header">` containing `<ul class="nav-links">`
- **Nav items (8 total):** home, publications, working papers, teaching, workshops, curriculum vitae, causal inference: the mixtape, orley genealogy
- **Nav casing:** all lowercase
- **Content wrapper:** `<div class="container">` with `row` / `col-xs-12 col-md-8 offset-md-2` grid
- **Footer spacer:** `<div class="row" style="min-height: 200px;"></div>`

---

## Page-by-Page Findings

### 1. publications.html

| Attribute | Expected (per index.html) | Actual | Match? |
|-----------|--------------------------|--------|--------|
| CSS files | bootstrap.css + scunning.css | bootstrap.css + scunning.css | YES |
| Google Fonts | Merriweather, Source Sans Pro, Muli | Same | YES |
| Font Awesome | Yes | Yes | YES |
| Google Analytics | Yes | Yes | YES |
| Header name | `scott cunningham` (lowercase) | `Scott Cunningham` (title case) | **NO** |
| Header title | `ben h. williams professor of economics` | `professor of economics` | **NO** |
| Nav items | 8 items (includes CV and orley) | 7 items — **missing "curriculum vitae"** | **NO** |
| Nav casing | all lowercase | Title Case (`Home`, `Publications`, etc.) | **NO** |
| Footer spacer | Yes | **Missing** | **NO** |

**Issues:**
1. Header h1 uses title case instead of lowercase
2. Subtitle says "professor of economics" instead of "ben h. williams professor of economics"
3. Nav link labels use Title Case instead of lowercase
4. "curriculum vitae" nav link is entirely absent
5. No footer spacer div

---

### 2. working_papers.html

| Attribute | Expected | Actual | Match? |
|-----------|----------|--------|--------|
| CSS files | bootstrap.css + scunning.css | bootstrap.css + scunning.css | YES |
| Google Fonts | Same | Same | YES |
| Font Awesome | Yes | Yes | YES |
| Google Analytics | Yes | Yes | YES |
| Header name | `scott cunningham` (lowercase) | `Scott Cunningham` (title case) | **NO** |
| Header title | `ben h. williams professor of economics` | `professor of economics` | **NO** |
| Nav items | 8 items | 7 items — **missing "curriculum vitae"** | **NO** |
| Nav casing | all lowercase | Title Case | **NO** |
| Footer spacer | Yes | **Missing** | **NO** |

**Issues:** Same pattern as publications.html — title case name/nav, outdated subtitle, missing CV link, no footer spacer.

---

### 3. teaching.html

| Attribute | Expected | Actual | Match? |
|-----------|----------|--------|--------|
| CSS files | bootstrap.css + scunning.css | Same | YES |
| Google Fonts | Same | Same | YES |
| Font Awesome | Yes | Yes | YES |
| Google Analytics | Yes | Yes | YES |
| Header name | `scott cunningham` (lowercase) | `scott cunningham` (lowercase) | YES |
| Header title | `ben h. williams professor of economics` | `ben h. williams professor of economics` | YES |
| Nav items | 8 items | **7 items — missing "orley genealogy"** | **NO** |
| Nav casing | all lowercase | all lowercase | YES |
| Footer spacer | Yes | Yes | YES |

**Issues:**
1. Nav is missing the "orley genealogy" link (only 7 items instead of 8)
2. Otherwise the most consistent page with the homepage

---

### 4. workshops.html

| Attribute | Expected | Actual | Match? |
|-----------|----------|--------|--------|
| CSS files | bootstrap.css + scunning.css | bootstrap.css + scunning.css | YES |
| Google Fonts | Same | Same | YES |
| Font Awesome | Yes | **Missing** | **NO** |
| Google Analytics | Yes | **Missing** | **NO** |
| Header name | `scott cunningham` (lowercase) | `Scott Cunningham` (title case) | **NO** |
| Header title | `ben h. williams professor of economics` | `professor of economics` | **NO** |
| Nav items | 8 items | 8 items | YES |
| Nav casing | all lowercase | Title Case | **NO** |
| Footer spacer | Yes | **Missing** | **NO** |

**Issues:**
1. Font Awesome script tag is completely absent
2. Google Analytics script is completely absent
3. Header h1 uses title case
4. Subtitle is outdated ("professor of economics")
5. Nav labels use Title Case
6. No footer spacer div

---

### 5. cv.html — COMPLETELY DIFFERENT TEMPLATE

| Attribute | Expected | Actual | Match? |
|-----------|----------|--------|--------|
| CSS files | bootstrap.css + scunning.css | **`style.css` (does not exist!) + massive inline `<style>` block** | **NO** |
| Google Fonts | Merriweather, Source Sans Pro, Muli | **None — uses Georgia serif via inline CSS** | **NO** |
| Font Awesome | Yes | **Missing** | **NO** |
| Google Analytics | Yes | **Missing** | **NO** |
| Header structure | `<header class="container app-header">` | **Plain `<h1>` tag, no header element** | **NO** |
| Nav structure | `<ul class="nav-links">` in header | **Inline `<nav>` with pipe-separated links** | **NO** |
| Nav items | 8 items | **4 items only** (Home, Teaching, Mixtape Sessions, Orley Genealogy) | **NO** |
| Bootstrap grid | Yes | **None — uses `max-width: 900px; margin: auto`** | **NO** |
| Content structure | container/row/col | **`<section>` elements** | **NO** |
| Footer spacer | Yes | **Missing** | **NO** |
| Font family | Merriweather / Source Sans Pro | **Georgia, serif** | **NO** |
| Background | Per scunning.css | **Hardcoded `#fff`** | **NO** |
| Text color | Per scunning.css | **Hardcoded `#111`** | **NO** |

**This page is a total rebuild.** It shares nothing with the homepage template. Key issues:

1. Links to `style.css` which does not exist in the repository (dead reference)
2. All styling is done via a large inline `<style>` block
3. No bootstrap, no scunning.css, no shared fonts
4. Navigation is a completely different pattern (centered text with pipe separators)
5. Nav is missing publications, working papers, workshops, and curriculum vitae links
6. Uses `<section>` elements instead of bootstrap grid rows/columns
7. Font is Georgia instead of Merriweather/Source Sans Pro
8. No Font Awesome, no Google Analytics
9. Has an HTML comment at line 1 before the DOCTYPE that should be removed
10. Exposes personal phone numbers (mobile, office, fax) which are not shown on any other page — verify this is intentional

---

### 6. orley.html

| Attribute | Expected | Actual | Match? |
|-----------|----------|--------|--------|
| CSS files | bootstrap.css + scunning.css | bootstrap.css + scunning.css | YES |
| Google Fonts | Same | Same | YES |
| Font Awesome | Yes | Yes | YES |
| Google Analytics | Yes | Yes | YES |
| Header name | `scott cunningham` (lowercase) | `Scott Cunningham` (title case) | **NO** |
| Header title | `ben h. williams professor of economics` | `ben h. williams professor of economics` | YES |
| Nav items | 8 items | **6 items — missing "teaching" and "curriculum vitae"** | **NO** |
| Nav casing | all lowercase | Title Case | **NO** |
| Footer spacer | Yes | Unknown (not checked to end) | — |
| Inline styles | Minimal | **Large inline `<style>` block (~250 lines)** | **NOTE** |

**Issues:**
1. Header h1 uses title case
2. Nav labels use Title Case
3. Nav is missing "teaching" and "curriculum vitae" links
4. Contains a very large inline `<style>` block for page-specific styles (orley-hero, stats, tree, form, search components) — this is acceptable for page-specific features but should ideally be extracted to a separate CSS file or added to scunning.css

---

## Summary of Inconsistencies

### A. Header Name Casing

| Page | h1 text | Correct? |
|------|---------|----------|
| index.html | `scott cunningham` | YES (reference) |
| publications.html | `Scott Cunningham` | NO |
| working_papers.html | `Scott Cunningham` | NO |
| teaching.html | `scott cunningham` | YES |
| workshops.html | `Scott Cunningham` | NO |
| cv.html | `Scott Cunningham` | NO |
| orley.html | `Scott Cunningham` | NO |

**Fix needed on 5 pages:** Change to lowercase `scott cunningham`.

### B. Header Subtitle

| Page | Subtitle | Correct? |
|------|----------|----------|
| index.html | `ben h. williams professor of economics` | YES (reference) |
| publications.html | `professor of economics` | NO |
| working_papers.html | `professor of economics` | NO |
| teaching.html | `ben h. williams professor of economics` | YES |
| workshops.html | `professor of economics` | NO |
| cv.html | (no subtitle — different template) | NO |
| orley.html | `ben h. williams professor of economics` | YES |

**Fix needed on 3 pages:** Update to full title.

### C. Navigation Link Completeness

The homepage has 8 nav links. Other pages are missing various links:

| Page | Missing Links |
|------|--------------|
| publications.html | curriculum vitae |
| working_papers.html | curriculum vitae |
| teaching.html | orley genealogy |
| workshops.html | (none — has all 8) |
| cv.html | publications, working papers, workshops, curriculum vitae |
| orley.html | teaching, curriculum vitae |

### D. Navigation Link Casing

| Page | Casing | Correct? |
|------|--------|----------|
| index.html | lowercase | YES (reference) |
| publications.html | Title Case | NO |
| working_papers.html | Title Case | NO |
| teaching.html | lowercase | YES |
| workshops.html | Title Case | NO |
| cv.html | Title Case | NO |
| orley.html | Title Case | NO |

### E. Missing Font Awesome

- workshops.html: missing
- cv.html: missing

### F. Missing Google Analytics

- workshops.html: missing
- cv.html: missing

### G. Missing Footer Spacer

- publications.html: missing
- working_papers.html: missing
- workshops.html: missing
- cv.html: missing

### H. cv.html is a Completely Different Template

This is the single largest inconsistency. The entire page needs to be rebuilt using the shared template (bootstrap.css, scunning.css, shared header/nav, Google Fonts, Font Awesome, Google Analytics).

---

## Recommended Actions (Priority Order)

### Priority 1: Rebuild cv.html
- Replace with the standard template (bootstrap + scunning.css + shared header/nav)
- Remove the dead `style.css` link
- Remove all inline styles
- Use Merriweather/Source Sans Pro fonts
- Add Font Awesome and Google Analytics
- Add all 8 nav links in lowercase
- Port the CV content into bootstrap grid layout
- Decide whether to keep phone numbers visible

### Priority 2: Standardize headers across all pages
- Set h1 to `scott cunningham` (lowercase) on all pages
- Set subtitle to `ben h. williams professor of economics` on all pages

### Priority 3: Standardize navigation across all pages
- Ensure all 8 nav links appear on every page
- Ensure all nav link text is lowercase
- Ensure the correct `class="in"` is set on the current page's link

### Priority 4: Add missing infrastructure
- Add Font Awesome to workshops.html
- Add Google Analytics to workshops.html
- Add footer spacer div to publications.html, working_papers.html, workshops.html

### Priority 5: Consider extracting orley.html inline styles
- The ~250 lines of inline CSS could be moved to a dedicated `css/orley.css` file to keep the HTML cleaner, though this is cosmetic and low priority

---

## Additional Pages Not Fully Audited

The following HTML files also exist in the repository and should be checked for consistency:
- `gov2001.html`
- `gov51.html`
- `mixtape.html`
- `JHR_Threads.html`

These were outside the scope of this audit but may have similar issues.
