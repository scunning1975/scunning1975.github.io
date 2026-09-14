# =====================================================================
#  THE PADDING PARADOX  ·  COACH THE DECADES
#  A toy Shiny app inspired by a group-chat conversation.
#  Nothing here is real data; every number is an assumption you can move.
#
#  Two tabs:
#    Play     four moments when football changed; you make the call
#    Explore  the original pads-vs-speed simulator
# =====================================================================

library(shiny)
library(ggplot2)

DEFAULTS <- list(
  speed_cost_per_lb = 0.03,
  injury_k          = 0.15,
  base_injury       = 0.03,
  yards_per_tenth   = 1.5,
  base_40_by_pos    = c("WR / CB" = 4.45,
                        "RB / LB" = 4.60,
                        "OL / DL" = 5.10)
)

ERAS <- data.frame(
  era  = c("1990s (heavy)", "2010s (transition)", "2020s (minimal)"),
  load = c(9.0,             4.0,                   1.5)
)

BAYLOR_GREEN <- "#154734"
BAYLOR_GOLD  <- "#FFB81C"
BRICK        <- "#9B2C2C"

# ASCII: 1995 vs 2025 side by side, BBS style. 56 columns wide.
# Numbers below are WR/CB at the default assumptions:
#   40 = 4.45 + 0.03*load ; inj/100 = 100 * 0.03 * exp(-0.15*load)
#   1995 (9 lb): 4.72s, 0.8      2025 (1.5 lb): 4.50s, 2.4
ASCII_ART <- "╔══════════════════════════════════════════════════════╗
║                THE  PADDING  PARADOX                 ║
║             why modern players wear less             ║
╚══════════════════════════════════════════════════════╝

             ▄▄▄▄▄▄▄                 ▄▄▄▄▄▄▄
            ▐███████▌               ▐███████▌
            ▐█ o o █▌               ▐█ o o █▌
             ▀█▄▄▄█▀                 ▀█▄▄▄█▀
     ╔═══════╗ ▐█▌ ╔═══════╗           ▐█▌
     ║▓▓▓▓▓▓▓╚═╧═╧═╝▓▓▓▓▓▓▓║        ┌──╧═╧──┐
     ║▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓║        │▒▒▒▒▒▒▒│
     ╚═══╗▓▓▓▓ #12 ▓▓▓▓╔═══╝  vs    │▒▒#12▒▒│
         ║░░░░░░░░░░░░░║            │▒▒▒▒▒▒▒│
         ║░░░░░░░░░░░░░║            │░░░░░░░│
         ╚══╤══╤═╤══╤══╝            └─┬───┬─┘
            ▐██▌ ▐██▌                 │   │
            ▐██▌ ▐██▌                 │   │
             ││   ││                  │   │
            ▐██▌ ▐██▌                 │   │
             ││   ││                  │   │
            ═╧╧═ ═╧╧═                ═╧═ ═╧═

     1995 · SPLASH                 2025 · SPEED
     9 lb of pads                  1.5 lb of pads
     4.72s · 0.8 inj / 100         4.50s · 2.4 inj / 100"

# ---------------------------------------------------------------- CSS
CSS <- "
  body { background: #FBFAF7; font-family: 'Helvetica Neue', sans-serif; }
  h1, h2, h3, h4 { color: %GREEN%; }
  .big { font-size: 40px; font-weight: 700; color: %GREEN%; line-height: 1.1;
         margin-top: 6px; }
  .small-caps { text-transform: uppercase; letter-spacing: 0.08em;
                color: #6B7280; font-size: 12px; font-weight: 600; }
  .card { background: white; padding: 18px 20px; border-radius: 8px;
          border-left: 4px solid %GOLD%; margin-bottom: 12px; }
  .tiles { display: flex; flex-wrap: wrap; }
  .tiles > div { display: flex; }
  .tiles .card { flex: 1; }
  .ascii { background: #0C2E21; color: %GOLD%; padding: 22px 18px;
           margin: 0; border-radius: 8px; text-align: center;
           font-family: 'Menlo','Courier New',monospace;
           font-size: 13px; line-height: 1.2; white-space: pre;
           overflow-x: auto; border: 0; }
  .ascii .art { display: inline-block; text-align: left; }
  @media (max-width: 600px) { .ascii { font-size: 9px; padding: 14px 8px; } }
  .howto { background: white; padding: 16px 22px; border-radius: 8px;
           border-left: 4px solid %GOLD%; margin: 14px 0 18px 0;
           font-size: 15px; line-height: 1.5; }
  .howto ol { margin: 6px 0 0 0; padding-left: 20px; }
  .howto li { margin-bottom: 4px; }
  .howto .lead { color: #1A1A1A; margin: 4px 0 6px 0; }
  .era-note { display: block; margin-top: 10px; color: #6B7280; }
  .btn-primary { background: %GREEN%; border-color: %GREEN%; }
  .btn-primary:hover { background: #0C2E21; border-color: #0C2E21; }

  .nav-tabs { margin-top: 16px; border-bottom: 2px solid #E5E1D8; }
  .nav-tabs > li > a { color: #6B7280; font-weight: 700; letter-spacing: .06em;
                       text-transform: uppercase; font-size: 13px; border: 0; }
  .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus { color: %GREEN%; border: 0;
                       border-bottom: 3px solid %GOLD%; background: transparent; }
  .tab-content { padding-top: 14px; }

  .strip { display: flex; justify-content: space-between; align-items: baseline;
           flex-wrap: wrap; gap: 8px; padding: 4px 4px 8px; }
  .strip .lvl { font-size: 22px; font-weight: 700; color: %GREEN%; }
  .strip .lvl span { color: #6B7280; font-weight: 500; font-size: 15px;
                     margin-left: 10px; }
  .strip .tot { font-family: Menlo, monospace; color: #6B7280; font-size: 14px; }
  .field { background: #0C2E21; color: %GOLD%; padding: 14px 18px;
           border-radius: 8px; font-family: 'Menlo','Courier New',monospace;
           font-size: 13px; line-height: 1.25; white-space: pre;
           overflow-x: auto; border: 0; margin: 10px 0 14px; }
  @media (max-width: 600px) { .field { font-size: 10px; padding: 10px; } }
  .situ { font-size: 16px; line-height: 1.55; color: #1A1A1A; margin: 0 0 12px; }
  .radio label { font-size: 15px; line-height: 1.4; }
  .reveal { background: white; padding: 16px 20px; border-radius: 8px;
            border-left: 4px solid %GOLD%; margin: 12px 0; font-size: 15.5px;
            line-height: 1.55; }
  .reveal p { margin: 6px 0 0; }
  .pts { font-size: 34px; font-weight: 700; color: %GREEN%; line-height: 1.1; }
  .grade { font-size: 44px; font-weight: 700; color: %GREEN%; line-height: 1.1; }
  .share { background: #0C2E21; color: %GOLD%; padding: 14px 16px;
           border-radius: 8px; font-family: Menlo, monospace; font-size: 13px;
           white-space: pre; overflow-x: auto; margin: 0; }
  .flexrow { display: flex; justify-content: space-between; align-items: center;
             flex-wrap: wrap; gap: 12px; }
"
CSS <- gsub("%GOLD%", BAYLOR_GOLD, gsub("%GREEN%", BAYLOR_GREEN, CSS, fixed = TRUE),
            fixed = TRUE)

# ---------------------------------------------------------------- levels
LEVELS <- list(
  list(
    name = "PADS", era = "1990s", better = "higher", metric = "season yards",
    choices = c("Heavy — 9 lb, the 1990s standard"    = "9",
                "Medium — 4 lb"                       = "4",
                "Light — 1.5 lb, what they wear now"  = "1.5"),
    short   = c("9" = "Heavy  9 lb", "4" = "Medium  4 lb", "1.5" = "Light  1.5 lb"),
    situation = paste(
      "It's the 1990s. Your star receiver runs a 4.45, and the equipment room",
      "has three shoulder-pad setups. Heavier protects him. Lighter makes him",
      "faster. It's a 300-play season, and an injury costs him plays — sometimes",
      "a lot of them. Pick his pads."),
    ascii = "        ▄▄▄▄▄▄▄
       ▐███████▌         your star receiver
       ▐█ o o █▌         4.45 forty · 300 plays
        ▀█▄▄▄█▀
     ╔═══╗ ▐█▌ ╔═══╗
     ║▓▓▓╚═╧═╧═╝▓▓▓║     ← how much goes here?
     ╚═╗▓▓ #12 ▓▓╔═╝
       ║░░░░░░░░░║
       ╚═╤═════╤═╝
        ▐█▌   ▐█▌        ← and here?
        ═╧═   ═╧═",
    reveal = paste(
      "Light usually wins. The yards he gains on every play outrun the games he",
      "occasionally loses — but not always, and that's the gamble every player",
      "started taking once the passing game made speed worth more than",
      "protection. Knee, thigh and hip pads went first: they protected the least",
      "and slowed the most.")
  ),
  list(
    name = "THREE RECEIVERS", era = "2010s", better = "lower",
    metric = "yards allowed per play",
    choices = c("Stay in base 4-3 — three linebackers"            = "base",
                "Go nickel — pull a linebacker, add a corner"      = "nickel",
                "Invent the STAR — a safety-corner hybrid"         = "star"),
    short   = c(base = "Base 4-3", nickel = "Nickel", star = "STAR"),
    situation = paste(
      "It's the 2010s. The offense lines up with three receivers on 60% of",
      "snaps and runs power on the rest. Your base defense has three",
      "linebackers: great against the run, slow against that third receiver.",
      "What personnel do you send out?"),
    ascii = "  offense    O                O O O O O                O
                                    Q                     ▲
                        O           B                  3rd WR

  you        X   X    X     X   X    X     X   X    X
                    X            ?            X
                                 ▲
                      linebacker, corner, or something new?",
    reveal = paste(
      "Nickel became the base defense because three receivers became the base",
      "offense — and the STAR was the hedge: safety size, corner speed, built",
      "for a game that keeps making you choose. It's also why slot receivers",
      "are shorter. Inside there's space and no sideline, so quickness beats",
      "height.")
  ),
  list(
    name = "THE RPO", era = "huddle era → 2010s", better = "lower",
    metric = "yards allowed per play", slider = TRUE,
    situation = paste(
      "First, the huddle era. You've scouted them: on this down they run 65% of",
      "the time. You're the linebacker — set how often you fit the run versus",
      "drop into coverage. Then watch what the RPO does to that decision."),
    ascii = "              O   O O O O O
                       Q  ← in the huddle era, he already decided
                       B     in the RPO era, he watches YOU

         X    X    X    X
              [ YOU ]   ← fit the run, or drop?
            X          X",
    reveal = paste(
      "In the huddle era your read was worth more than a yard a play — scouting",
      "tendencies won. The RPO took that away: the quarterback decides after",
      "the snap by reading you, so whatever you do is what he wanted. Defenses",
      "answered the only way left — stop guessing and start disguising.")
  ),
  list(
    name = "THE EDGE", era = "2020s", better = "lower",
    metric = "yards allowed per play",
    choices = c("The run-stuffer — 300 lb, immovable"                    = "stuffer",
                "The balanced end — 275 lb"                              = "balanced",
                "The speed rusher — 255 lb, first step of a receiver"    = "speed"),
    short   = c(stuffer = "Run-stuffer  300", balanced = "Balanced  275",
                speed = "Speed rusher  255"),
    situation = paste(
      "It's the 2020s. The ball comes out in 2.5 seconds and they throw on 65%",
      "of snaps. You have one roster spot on the edge. Who do you take?"),
    ascii = "                  O  O  O  O  O          ball out in 2.5 s
                          Q               65% pass
                          B

        ?   X  X  X  X   ?
        ▲                ▲
      the edge         the edge",
    reveal = paste(
      "'Defensive end' described where a man lined up. 'Edge rusher' describes",
      "what he's for. The name changed when the job changed — and the job",
      "changed when the ball started leaving in 2.5 seconds. Same level, same",
      "luck, 1990s play-calling: the 300-pounder wins.")
  )
)

# ---------------------------------------------------------------- engine
# One draw of luck per level, shared across every option so the comparison
# is a counterfactual: same bounces, different decision.
luck_draw <- function(n, seed) {
  set.seed(seed)
  list(z = rnorm(n), u = runif(n), v = runif(n), w = runif(n))
}

lvl1_run <- function(load, L) {
  n        <- length(L$z)
  ypp      <- 5.0 - 0.20 * load
  inj_rate <- 0.009 * exp(-0.15 * load)
  injured  <- L$u < inj_rate
  miss_len <- c(5, 10, 20, 40, 80, 150)[
    pmin(findInterval(L$v, c(.35, .60, .80, .90, .96)) + 1, 6)]
  avail <- rep(TRUE, n); out_until <- 0; n_inj <- 0
  for (i in seq_len(n)) {
    if (i <= out_until) { avail[i] <- FALSE; next }
    if (injured[i]) { n_inj <- n_inj + 1; out_until <- i + miss_len[i] }
  }
  yards <- ypp + 4 * L$z
  list(value = sum(yards[avail]), injuries = n_inj, missed = sum(!avail))
}

lvl2_run <- function(personnel, L) {
  allow <- list(base   = c(spread = 8.5, power = 3.0),
                nickel = c(spread = 5.5, power = 5.5),
                star   = c(spread = 6.2, power = 4.0))[[personnel]]
  spread <- L$u < 0.60
  mean(ifelse(spread, allow["spread"], allow["power"]) + 5 * L$z)
}

lvl3_curve <- function(t, L, era) {
  fit <- L$v < t
  if (era == "huddle") {
    run <- L$u < 0.65
    y <- ifelse(run, ifelse(fit, 2.5, 7.0), ifelse(fit, 9.0, 4.0))
  } else {
    y <- ifelse(fit, 6.5, 6.0)      # he reads you: fit → he throws, drop → he hands off
  }
  mean(y + 3 * L$z)
}

lvl4_run <- function(de, L, pass_rate) {
  pr    <- c(stuffer = 0.04, balanced = 0.10, speed = 0.18)[[de]]
  runy  <- c(stuffer = 3.0,  balanced = 4.0,  speed = 5.0)[[de]]
  pass  <- L$u < pass_rate
  press <- L$v < pr
  mean(ifelse(pass, ifelse(press, -5, 7.5), runy) + 3 * L$z)
}

score_pts <- function(your, best, better) {
  p <- if (better == "higher") 100 * your / best else 100 * best / your
  as.integer(min(100, max(0, round(p))))
}

run_level <- function(i, choice, seed) {
  lv <- LEVELS[[i]]
  n  <- if (i == 1) 300 else 200
  L  <- luck_draw(n, seed)
  res <- list(i = i, choice = choice, better = lv$better, metric = lv$metric)

  if (i == 1) {
    keys <- c("9", "4", "1.5")
    vals <- vapply(keys, function(k) lvl1_run(as.numeric(k), L)$value, numeric(1))
    det  <- lvl1_run(as.numeric(choice), L)
    res$extra <- sprintf("%d injur%s · %d plays missed", det$injuries,
                         if (det$injuries == 1) "y" else "ies", det$missed)
    fmt <- function(v) paste(format(round(v), big.mark = ","), "yds")
  } else if (i == 2) {
    keys <- c("base", "nickel", "star")
    vals <- vapply(keys, lvl2_run, numeric(1), L = L)
    fmt  <- function(v) sprintf("%.2f", v)
  } else if (i == 3) {
    grid <- seq(0, 1, by = 0.05)
    hud  <- vapply(grid, lvl3_curve, numeric(1), L = L, era = "huddle")
    rpo  <- vapply(grid, lvl3_curve, numeric(1), L = L, era = "rpo")
    t    <- as.numeric(choice) / 100
    your <- lvl3_curve(t, L, "huddle")
    res$curve <- data.frame(t = grid, huddle = hud, rpo = rpo)
    res$t     <- t
    res$your  <- your
    res$best  <- min(hud)
    res$rpo_you <- lvl3_curve(t, L, "rpo")
    res$points <- score_pts(your, res$best, "lower")
    res$your_txt <- sprintf("%.2f", your)
    res$extra <- sprintf("best tendency this season: fit %d%% (%.2f)",
                         round(100 * grid[which.min(hud)]), res$best)
    return(res)
  } else {
    keys <- c("stuffer", "balanced", "speed")
    vals <- vapply(keys, lvl4_run, numeric(1), L = L, pass_rate = 0.65)
    v90  <- vapply(keys, lvl4_run, numeric(1), L = L, pass_rate = 0.40)
    res$era90_txt <- sprintf(
      "Same luck, 1990s play-calling (40%% pass): %s",
      paste(sprintf("%s %.2f", lv$short[keys], v90), collapse = "  ·  "))
    fmt <- function(v) sprintf("%.2f", v)
  }

  best <- if (lv$better == "higher") max(vals) else min(vals)
  res$df <- data.frame(key = keys, label = unname(lv$short[keys]), value = vals,
                       you = keys == choice, best = vals == best,
                       txt = vapply(vals, fmt, character(1)),
                       stringsAsFactors = FALSE)
  res$your     <- vals[[choice]]
  res$best     <- best
  res$your_txt <- fmt(res$your)
  res$points   <- score_pts(res$your, best, lv$better)
  res
}

grade_for <- function(total) {
  if (total >= 370) "Hall of Fame coordinator"
  else if (total >= 330) "Head coach"
  else if (total >= 280) "Position coach"
  else if (total >= 220) "Grad assistant"
  else "Sports-radio caller"
}

# ---------------------------------------------------------------- plots
theme_pp <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold", color = BAYLOR_GREEN),
          legend.position = "bottom")
}

plot_level <- function(res) {
  if (res$i == 3) {
    d  <- res$curve
    dl <- rbind(
      data.frame(t = d$t, y = d$huddle, era = "Huddle era — they run 65%"),
      data.frame(t = d$t, y = d$rpo,    era = "RPO era — the QB reads you"))
    pts <- data.frame(t = res$t, y = c(res$your, res$rpo_you),
                      lab = c("you, huddle era", "you, RPO era"))
    return(
      ggplot(dl, aes(t * 100, y, color = era)) +
        geom_line(linewidth = 1.7) +
        geom_point(data = pts, aes(t * 100, y), inherit.aes = FALSE,
                   color = BAYLOR_GOLD, size = 5) +
        geom_text(data = pts, aes(t * 100, y, label = lab), inherit.aes = FALSE,
                  color = "#6B7280", size = 3.8, vjust = -1.1) +
        scale_color_manual(values = c(BAYLOR_GREEN, BRICK)) +
        scale_y_continuous(expand = expansion(mult = c(.08, .22))) +
        labs(x = "How often you fit the run (%)", y = "Yards allowed per play",
             color = NULL, title = "Your choice mattered. Then it didn't.") +
        theme_pp() + theme(panel.grid.major.y = element_line()))
  }
  d <- res$df
  d$label <- factor(d$label,
                    levels = d$label[order(d$value, decreasing = (res$better == "lower"))])
  d$txt[d$best] <- paste(d$txt[d$best], "· best")
  ggplot(d, aes(label, value, fill = you)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = txt), hjust = -0.12, size = 4.2, color = "#1A1A1A") +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = BAYLOR_GOLD, `FALSE` = BAYLOR_GREEN),
                      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, .32))) +
    labs(x = NULL, y = res$metric,
         title = "Same luck, every option — gold is yours") +
    theme_pp()
}

# ---------------------------------------------------------------- UI pieces
level_panel <- function(i, total) {
  lv <- LEVELS[[i]]
  control <- if (isTRUE(lv$slider)) {
    sliderInput(paste0("choice_", i), "How often do you fit the run?",
                min = 0, max = 100, value = 50, step = 5, post = "%",
                ticks = FALSE, width = "100%")
  } else {
    radioButtons(paste0("choice_", i), NULL, choices = lv$choices,
                 selected = character(0))
  }
  tagList(
    div(class = "strip",
        div(class = "lvl", sprintf("Level %d of 4 · %s", i, lv$name),
            span(lv$era)),
        div(class = "tot", sprintf("score so far  %d / 400", total))),
    div(class = "card",
        HTML(sprintf('<pre class="field">%s</pre>', lv$ascii)),
        p(class = "situ", lv$situation),
        control,
        actionButton(paste0("run_", i), "Run the plays", class = "btn-primary"))
  )
}

final_panel <- function(scores) {
  total <- sum(scores)
  names <- vapply(LEVELS, `[[`, character(1), "name")
  line2 <- paste(sprintf("%s %d", c("Pads", "Three WR", "RPO", "Edge"), scores),
                 collapse = "  ·  ")
  share <- sprintf("COACH THE DECADES  ·  %d / 400  ·  %s\n%s\nscunning.com/apps/padding-paradox",
                   total, grade_for(total), line2)
  tagList(
    div(class = "strip", div(class = "lvl", "Final"), div(class = "tot", "")),
    div(class = "card",
        div(class = "small-caps", "your grade"),
        div(class = "grade", grade_for(total)),
        div(class = "big", style = "font-size: 26px;", sprintf("%d / 400", total)),
        tableOutput("final_table")),
    div(class = "card",
        div(class = "small-caps", "paste this into the chat"),
        tags$pre(class = "share", share),
        br(),
        actionButton("again", "Play again", class = "btn-primary"))
  )
}

# ---------------------------------------------------------------- UI
ui <- fluidPage(
  title = "The Padding Paradox",
  tags$head(tags$style(HTML(CSS))),

  # Built with HTML() so htmltools cannot indent inside the <pre>;
  # any whitespace there would shift the centered art.
  fluidRow(column(12, HTML(sprintf(
    '<pre class="ascii"><span class="art">%s</span></pre>', ASCII_ART)))),

  tabsetPanel(id = "tabs",
    tabPanel("Play · Coach the Decades",
      div(class = "howto",
          div(class = "small-caps", "the game"),
          p(class = "lead",
            "Four moments when football changed. In each one you're the coach ",
            "who has to decide. Pick, run the plays, and find out why the game ",
            "went the way it did — the same luck is applied to every option, so ",
            "you see what would have happened if you'd chosen differently.")),
      uiOutput("level_ui"),
      uiOutput("result_ui")
    ),
    tabPanel("Explore · The simulator",
      div(class = "howto",
        div(class = "small-caps", "how to use this"),
        p(class = "lead",
          "More padding means fewer injuries but slower feet. ",
          "Set the pads and watch what happens."),
        tags$ol(
          tags$li(strong("Pick a position"), " and drag the two pad sliders; ",
                  "the three tiles update live."),
          tags$li(strong("Hit Run"), " to simulate a game of plays and see ",
                  "the spread of outcomes, not just the average."),
          tags$li(strong("Scroll down"), " to compare your build against ",
                  "1990s, 2010s, and 2020s players with the same body."),
          tags$li(strong("Then break it."), " The two ", em("assumption"),
                  " sliders change how much speed a pound costs and how much ",
                  "protection it buys.")
        )
      ),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          div(class = "small-caps", "position"),
          selectInput("pos", NULL,
                      choices = names(DEFAULTS$base_40_by_pos),
                      selected = "WR / CB"),
          div(class = "small-caps", "pad loadout (lbs)"),
          sliderInput("shoulder", "Shoulder", 0, 10, 3, step = 0.5,
                      ticks = FALSE),
          sliderInput("lower",    "Thigh + hip + knee", 0, 8, 1, step = 0.5,
                      ticks = FALSE),
          hr(),
          div(class = "small-caps", "model assumptions"),
          sliderInput("speed_cost", "Speed cost (sec / lb)",
                      0, 0.10, DEFAULTS$speed_cost_per_lb, step = 0.005,
                      ticks = FALSE),
          sliderInput("injury_k", "Injury protection strength",
                      0, 0.40, DEFAULTS$injury_k, step = 0.01,
                      ticks = FALSE),
          hr(),
          numericInput("n_plays", "Plays to simulate", 1000,
                       min = 100, max = 20000, step = 100),
          fluidRow(
            column(6, actionButton("go",   "Run simulation",
                                   class = "btn-primary", width = "100%")),
            column(6, actionButton("reset", "Reset to 2020s",
                                   width = "100%"))
          )
        ),
        mainPanel(
          width = 8,
          fluidRow(class = "tiles",
            column(4, div(class = "card",
                          div(class = "small-caps", "40-yard time"),
                          div(class = "big", textOutput("speed_txt")))),
            column(4, div(class = "card",
                          div(class = "small-caps", "injuries / 100 plays"),
                          div(class = "big", textOutput("inj_txt")))),
            column(4, div(class = "card",
                          div(class = "small-caps", "avg yards / play"),
                          div(class = "big", textOutput("ypp_txt"))))
          ),
          div(class = "card", plotOutput("yards_hist", height = "280px")),
          div(class = "card",
              h4("Era comparison"),
              div(class = "small-caps",
                  "same position, same physics — only the pads change"),
              tableOutput("era_table"),
              tags$small(class = "era-note",
                         "Nothing above is measured. Every number is an assumption ",
                         "you can move. The point is the shape of the tradeoff, ",
                         "not the level."))
        )
      )
    )
  )
)

sim_player <- function(load_lbs, base_40, assump, n_plays = 1000) {
  eff_40 <- base_40 + assump$speed_cost_per_lb * load_lbs
  speed_gap    <- eff_40 - base_40          # seconds the pads cost this body
  ypp_mean     <- 5.5 - speed_gap * assump$yards_per_tenth * 10
  injury_rate  <- assump$base_injury * exp(-assump$injury_k * load_lbs)
  list(
    eff_40 = eff_40, injury_rate = injury_rate, ypp_mean = ypp_mean,
    plays = data.frame(
      yards  = rnorm(n_plays, mean = ypp_mean, sd = 4),
      injury = rbinom(n_plays, 1, injury_rate)
    )
  )
}

# ---------------------------------------------------------------- server
server <- function(input, output, session) {

  # ---- Play ---------------------------------------------------------------
  lvl      <- reactiveVal(1L)
  scores   <- reactiveVal(rep(0L, 4))
  last     <- reactiveVal(NULL)
  finished <- reactiveVal(FALSE)

  output$level_ui <- renderUI({
    if (finished()) final_panel(scores()) else level_panel(lvl(), sum(scores()))
  })

  lapply(1:4, function(i) {
    observeEvent(input[[paste0("run_", i)]], {
      if (!isTRUE(input[[paste0("run_", i)]] > 0)) return()
      ch <- input[[paste0("choice_", i)]]
      if (is.null(ch) || !length(ch)) {
        showNotification("Pick one first.", type = "message", duration = 2)
        return()
      }
      res <- run_level(i, as.character(ch), sample.int(1e6, 1))
      s <- scores(); s[i] <- res$points; scores(s)
      last(res)
    })
  })

  output$result_ui <- renderUI({
    res <- last(); req(res, !finished())
    lv <- LEVELS[[res$i]]
    tagList(
      div(class = "card",
          div(class = "small-caps", paste("your", lv$metric)),
          div(class = "flexrow",
              div(class = "big", res$your_txt),
              div(style = "color:#6B7280; font-size:15px;", res$extra)),
          plotOutput("lvl_plot", height = "270px")),
      div(class = "reveal",
          div(class = "small-caps", paste("why the game changed ·", lv$era)),
          p(lv$reveal),
          if (!is.null(res$era90_txt)) tags$small(class = "era-note", res$era90_txt)),
      div(class = "card flexrow",
          div(div(class = "small-caps", "points this level"),
              div(class = "pts", sprintf("%d / 100", res$points)),
              tags$small(style = "color:#6B7280;",
                         "Run again for new luck — the last run counts.")),
          if (res$i < 4) actionButton("next_lvl", "Next level  →", class = "btn-primary")
          else           actionButton("finish",   "See your grade  →", class = "btn-primary"))
    )
  })

  output$lvl_plot <- renderPlot({ res <- last(); req(res); plot_level(res) })

  observeEvent(input$next_lvl, {
    if (!isTRUE(input$next_lvl > 0)) return()
    last(NULL); lvl(min(4L, lvl() + 1L))
  })
  observeEvent(input$finish, {
    if (!isTRUE(input$finish > 0)) return()
    last(NULL); finished(TRUE)
  })
  observeEvent(input$again, {
    if (!isTRUE(input$again > 0)) return()
    scores(rep(0L, 4)); last(NULL); lvl(1L); finished(FALSE)
  })

  output$final_table <- renderTable({
    data.frame(Level = vapply(LEVELS, `[[`, character(1), "name"),
               Era   = vapply(LEVELS, `[[`, character(1), "era"),
               Points = sprintf("%d / 100", scores()),
               check.names = FALSE)
  }, striped = TRUE, align = "llr")

  # ---- Explore ------------------------------------------------------------
  observeEvent(input$reset, {
    updateSliderInput(session, "shoulder", value = 1.0)   # 1.0 + 0.5 = 1.5 lb,
    updateSliderInput(session, "lower",    value = 0.5)   # the 2020s era row
  })
  assump <- reactive(list(
    speed_cost_per_lb = input$speed_cost,
    injury_k          = input$injury_k,
    base_injury       = DEFAULTS$base_injury,
    yards_per_tenth   = DEFAULTS$yards_per_tenth
  ))
  base_40 <- reactive(DEFAULTS$base_40_by_pos[[input$pos]])
  load_lbs <- reactive(input$shoulder + input$lower)
  live <- reactive(sim_player(load_lbs(), base_40(), assump(), 1))
  sim  <- eventReactive(input$go,
                        sim_player(load_lbs(), base_40(), assump(),
                                   input$n_plays),
                        ignoreNULL = FALSE)

  output$speed_txt <- renderText(sprintf("%.2fs", live()$eff_40))
  output$inj_txt   <- renderText(sprintf("%.1f",  live()$injury_rate * 100))
  output$ypp_txt   <- renderText(sprintf("%.1f",  live()$ypp_mean))

  output$yards_hist <- renderPlot({
    d <- sim()$plays; m <- mean(d$yards)
    hj <- if (m > mean(range(d$yards))) 1.1 else -0.1
    ggplot(d, aes(yards)) +
      geom_histogram(bins = 40, fill = BAYLOR_GREEN, color = "white",
                     linewidth = 0.3) +
      geom_vline(xintercept = m, color = BAYLOR_GOLD, linewidth = 1.4) +
      annotate("text", x = m, y = Inf, vjust = 1.6, hjust = hj,
               label = sprintf("mean %.1f yds", m),
               color = BAYLOR_GOLD, fontface = "bold", size = 4.5) +
      labs(title = sprintf("%s plays simulated  ·  %d injuries",
                           format(nrow(d), big.mark = ","), sum(d$injury)),
           x = "Yards on the play", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", color = BAYLOR_GREEN))
  })

  output$era_table <- renderTable({
    do.call(rbind, lapply(seq_len(nrow(ERAS)), function(i) {
      s <- sim_player(ERAS$load[i], base_40(), assump(), 1)
      data.frame(
        Era              = ERAS$era[i],
        `Load (lb)`      = sprintf("%.1f", ERAS$load[i]),
        `40-time (s)`    = sprintf("%.2f", s$eff_40),
        `Yds/play`       = sprintf("%.1f", s$ypp_mean),
        `Inj / 100 plays`= sprintf("%.1f", s$injury_rate * 100),
        check.names = FALSE)
    }))
  }, striped = TRUE, align = "lrrrr")
}

shinyApp(ui, server)
