# =====================================================================
#  THE PADDING PARADOX
#  A toy Shiny app inspired by a group-chat conversation.
#  Nothing here is real data; every number is an assumption you can move.
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

ui <- fluidPage(
  title = "The Padding Paradox",
  tags$head(tags$style(HTML(sprintf("
    body { background: #FBFAF7; font-family: 'Helvetica Neue', sans-serif; }
    h1, h2, h3, h4 { color: %s; }
    .big { font-size: 40px; font-weight: 700; color: %s; line-height: 1.1;
           margin-top: 6px; }
    .small-caps { text-transform: uppercase; letter-spacing: 0.08em;
                  color: #6B7280; font-size: 12px; font-weight: 600; }
    .card { background: white; padding: 18px 20px; border-radius: 8px;
            border-left: 4px solid %s; margin-bottom: 12px; }
    .tiles { display: flex; flex-wrap: wrap; }
    .tiles > div { display: flex; }
    .tiles .card { flex: 1; }
    .ascii { background: #0C2E21; color: #FFB81C; padding: 22px 18px;
             margin: 0; border-radius: 8px; text-align: center;
             font-family: 'Menlo','Courier New',monospace;
             font-size: 13px; line-height: 1.2; white-space: pre;
             overflow-x: auto; border: 0; }
    .ascii .art { display: inline-block; text-align: left; }
    @media (max-width: 600px) { .ascii { font-size: 9px; padding: 14px 8px; } }
    .howto { background: white; padding: 16px 22px; border-radius: 8px;
             border-left: 4px solid %s; margin: 14px 0 18px 0;
             font-size: 15px; line-height: 1.5; }
    .howto ol { margin: 6px 0 0 0; padding-left: 20px; }
    .howto li { margin-bottom: 4px; }
    .howto .lead { color: #1A1A1A; margin: 4px 0 6px 0; }
    .era-note { display: block; margin-top: 10px; color: #6B7280; }
    .btn-primary { background: %s; border-color: %s; }
    .btn-primary:hover { background: #0C2E21; border-color: #0C2E21; }
  ", BAYLOR_GREEN, BAYLOR_GREEN, BAYLOR_GOLD, BAYLOR_GOLD, BAYLOR_GREEN, BAYLOR_GREEN)))),

  # Built with HTML() so htmltools cannot indent inside the <pre>;
  # any whitespace there would shift the centered art.
  fluidRow(column(12, HTML(sprintf(
    '<pre class="ascii"><span class="art">%s</span></pre>', ASCII_ART)))),

  fluidRow(column(12, div(class = "howto",
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
  ))),

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

server <- function(input, output, session) {
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
    # put the label on whichever side of the mean line has more room
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
