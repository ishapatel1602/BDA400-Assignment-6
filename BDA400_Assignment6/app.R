# BDA400 - Assignment 6
# Technical Analysis using R, Visualization Phase
# Complete R Shiny portfolio dashboard

# Install once if required:
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "scales"))

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(scales)

# ---------- Helper functions ----------
fetch_stock_data <- function(symbol, from, to) {
  tryCatch({
    x <- suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to,
                                     auto.assign = FALSE, warnings = FALSE))
    if (NROW(x) == 0) stop("No observations were returned.")
    x <- na.omit(x)
    x
  }, error = function(e) {
    stop(paste0("Unable to download ", symbol, " from Yahoo Finance. ", e$message))
  })
}

convert_timeframe <- function(x, timeframe) {
  if (timeframe == "Weekly") {
    x <- to.weekly(x, indexAt = "lastof", drop.time = TRUE)
  } else if (timeframe == "Monthly") {
    x <- to.monthly(x, indexAt = "lastof", drop.time = TRUE)
  }
  x
}

make_analysis_df <- function(x, short_n, long_n, rsi_n, macd_fast, macd_slow, macd_signal) {
  d <- data.frame(
    Date = as.Date(index(x)),
    Open = as.numeric(Op(x)),
    High = as.numeric(Hi(x)),
    Low = as.numeric(Lo(x)),
    Close = as.numeric(Cl(x)),
    Volume = as.numeric(Vo(x)),
    stringsAsFactors = FALSE
  )

  d$ShortMA <- as.numeric(SMA(d$Close, n = short_n))
  d$LongMA  <- as.numeric(SMA(d$Close, n = long_n))
  d$RSI     <- as.numeric(RSI(d$Close, n = rsi_n))

  macd <- MACD(d$Close, nFast = macd_fast, nSlow = macd_slow,
               nSig = macd_signal, maType = "EMA", percent = FALSE)
  d$MACD <- as.numeric(macd[, 1])
  d$MACDSignal <- as.numeric(macd[, 2])

  # State rule: short MA above long MA = Buy regime; below = Sell regime.
  d$State <- ifelse(is.na(d$ShortMA) | is.na(d$LongMA), "Hold",
                    ifelse(d$ShortMA > d$LongMA, "Buy",
                           ifelse(d$ShortMA < d$LongMA, "Sell", "Hold")))

  # Event rule: annotate only true crossovers to keep the chart readable.
  prev_diff <- c(NA, head(d$ShortMA - d$LongMA, -1))
  curr_diff <- d$ShortMA - d$LongMA
  d$Signal <- "Hold"
  d$Signal[!is.na(prev_diff) & prev_diff <= 0 & curr_diff > 0] <- "Buy"
  d$Signal[!is.na(prev_diff) & prev_diff >= 0 & curr_diff < 0] <- "Sell"
  d
}

rescale_to_price <- function(values, price) {
  ok <- is.finite(values) & is.finite(price)
  out <- rep(NA_real_, length(values))
  if (sum(ok) > 1 && diff(range(values[ok])) != 0) {
    out[ok] <- scales::rescale(values[ok], to = range(price[is.finite(price)], na.rm = TRUE))
  }
  out
}

# ---------- User interface ----------
ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body { background-color: #f7f8fa; }\n    .well { background-color: white; }\n    h2 { margin-top: 8px; }\n  "))),
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range",
                     start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Time Frame",
                  choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
      selectInput("chart_type", "Chart Type",
                  choices = c("Line", "Candlestick", "Area"), selected = "Candlestick"),
      checkboxGroupInput("technical_indicators", "Technical Indicators",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      checkboxInput("show_signals", "Show Buy/Sell Annotations", TRUE),
      hr(),
      h4("Trading Rule Parameters"),
      numericInput("short_ma", "Short MA", 20, min = 2, max = 200),
      numericInput("long_ma", "Long MA", 50, min = 3, max = 300),
      numericInput("rsi_n", "RSI Period", 14, min = 2, max = 100),
      fluidRow(
        column(4, numericInput("macd_fast", "MACD Fast", 12, min = 2, max = 100)),
        column(4, numericInput("macd_slow", "MACD Slow", 26, min = 3, max = 150)),
        column(4, numericInput("macd_sig", "Signal", 9, min = 2, max = 100))
      ),
      actionButton("refresh", "Refresh Data", class = "btn-primary")
    ),
    mainPanel(
      uiOutput("status"),
      plotOutput("stock_chart", height = "650px"),
      h4("Most Recent Trading Status"),
      tableOutput("latest_status"),
      h4("Recent Buy/Sell Events"),
      tableOutput("signal_table")
    )
  )
)

# ---------- Server logic ----------
server <- function(input, output, session) {
  raw_data <- eventReactive(input$refresh, {
    req(input$symbol, input$date_range)
    validate(need(input$date_range[1] < input$date_range[2],
                  "The start date must be earlier than the end date."))
    withProgress(message = "Downloading market data...", value = 0.5, {
      fetch_stock_data(toupper(trimws(input$symbol)), input$date_range[1], input$date_range[2] + 1)
    })
  }, ignoreNULL = FALSE)

  analysis_data <- reactive({
    validate(
      need(input$short_ma < input$long_ma, "Short MA must be smaller than Long MA."),
      need(input$macd_fast < input$macd_slow, "MACD Fast must be smaller than MACD Slow.")
    )
    x <- convert_timeframe(raw_data(), input$time_frame)
    validate(need(NROW(x) > input$long_ma,
                  paste("Not enough observations for the selected Long MA.",
                        "Choose a longer date range, a shorter MA, or a finer time frame.")))
    make_analysis_df(x, input$short_ma, input$long_ma, input$rsi_n,
                     input$macd_fast, input$macd_slow, input$macd_sig)
  })

  output$status <- renderUI({
    d <- analysis_data()
    latest <- tail(d, 1)
    div(class = "alert alert-info",
        strong(paste0(toupper(trimws(input$symbol)), " | ")),
        paste0(input$time_frame, " data | ", nrow(d), " observations | Latest close: $",
               format(round(latest$Close, 2), nsmall = 2), " | MA state: ", latest$State))
  })

  output$stock_chart <- renderPlot({
    d <- analysis_data()
    p <- ggplot(d, aes(x = Date)) +
      labs(title = paste(toupper(trimws(input$symbol)), "Technical Analysis"),
           subtitle = paste(input$time_frame, "time frame - Yahoo Finance"),
           x = NULL, y = "Price (USD)", caption = "Educational technical-analysis dashboard; not investment advice.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close, colour = "Close"), linewidth = 0.7)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close, fill = "Close"), alpha = 0.25) +
        geom_line(aes(y = Close, colour = "Close"), linewidth = 0.6)
    } else {
      d$Up <- d$Close >= d$Open
      candle_width <- if (input$time_frame == "Daily") 0.7 else if (input$time_frame == "Weekly") 4 else 18
      up <- d[d$Up, ]
      down <- d[!d$Up, ]
      p <- p +
        geom_segment(data = up, aes(x = Date, xend = Date, y = Low, yend = High), colour = "#1b9e77", linewidth = 0.4) +
        geom_rect(data = up, aes(xmin = Date - candle_width/2, xmax = Date + candle_width/2,
                                 ymin = pmin(Open, Close), ymax = pmax(Open, Close)),
                  fill = "#1b9e77", colour = "grey30", linewidth = 0.2) +
        geom_segment(data = down, aes(x = Date, xend = Date, y = Low, yend = High), colour = "#d95f02", linewidth = 0.4) +
        geom_rect(data = down, aes(xmin = Date - candle_width/2, xmax = Date + candle_width/2,
                                   ymin = pmin(Open, Close), ymax = pmax(Open, Close)),
                  fill = "#d95f02", colour = "grey30", linewidth = 0.2)
    }

    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = ShortMA, colour = paste0("SMA ", input$short_ma)), linewidth = 0.7, na.rm = TRUE) +
        geom_line(aes(y = LongMA, colour = paste0("SMA ", input$long_ma)), linewidth = 0.7, na.rm = TRUE)
    }

    # RSI and MACD are rescaled to the price range so they can be overlaid on one chart,
    # as required by the assignment. Their native values remain available in the data.
    if ("RSI" %in% input$technical_indicators) {
      d$RSI_scaled <- rescale_to_price(d$RSI, d$Close)
      p <- p + geom_line(data = d, aes(y = RSI_scaled, colour = "RSI (scaled)"),
                         linewidth = 0.55, linetype = "dashed", na.rm = TRUE)
    }

    if ("MACD" %in% input$technical_indicators) {
      d$MACD_scaled <- rescale_to_price(d$MACD, d$Close)
      d$MACDSignal_scaled <- rescale_to_price(d$MACDSignal, d$Close)
      p <- p +
        geom_line(data = d, aes(y = MACD_scaled, colour = "MACD (scaled)"), linewidth = 0.55, na.rm = TRUE) +
        geom_line(data = d, aes(y = MACDSignal_scaled, colour = "MACD Signal (scaled)"),
                  linewidth = 0.55, linetype = "dotted", na.rm = TRUE)
    }

    if (isTRUE(input$show_signals)) {
      events <- d[d$Signal %in% c("Buy", "Sell"), ]
      if (nrow(events) > 0) {
        p <- p +
          geom_point(data = events, aes(y = Close, shape = Signal), size = 3, stroke = 1) +
          geom_text(data = events, aes(y = Close, label = Signal), vjust = -1,
                    fontface = "bold", size = 3, check_overlap = TRUE) +
          scale_shape_manual(values = c(Buy = 24, Sell = 25))
      }
    }

    # Only add a manual colour scale when non-candlestick colour legends are present.
    if (input$chart_type != "Candlestick" || length(input$technical_indicators) > 0) {
      p <- p + scale_colour_discrete(name = "Series")
    }
    p
  })

  output$latest_status <- renderTable({
    d <- analysis_data()
    x <- tail(d, 1)
    data.frame(
      Date = x$Date,
      Close = round(x$Close, 2),
      Short_MA = round(x$ShortMA, 2),
      Long_MA = round(x$LongMA, 2),
      RSI = round(x$RSI, 2),
      MACD = round(x$MACD, 3),
      State = x$State,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$signal_table <- renderTable({
    d <- analysis_data()
    events <- d[d$Signal %in% c("Buy", "Sell"), c("Date", "Close", "ShortMA", "LongMA", "Signal")]
    if (nrow(events) == 0) return(data.frame(Message = "No crossover events occurred in the selected period."))
    events <- tail(events, 10)
    events$Close <- round(events$Close, 2)
    events$ShortMA <- round(events$ShortMA, 2)
    events$LongMA <- round(events$LongMA, 2)
    names(events) <- c("Date", "Close", "Short MA", "Long MA", "Signal")
    events
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
