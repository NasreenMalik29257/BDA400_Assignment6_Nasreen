
# BDA400 Assignment 6
# Stock Market Technical Analysis Dashboard
# Student: Nasreen Malik

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)
library(xts)

# -------------------------------
# 1. Data preparation
# -------------------------------

get_stock_prices <- function(symbol) {
  prices <- tryCatch(
    getSymbols(
      symbol,
      src = "yahoo",
      from = "2015-01-01",
      auto.assign = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(prices) || NROW(prices) == 0) {
    return(NULL)
  }
  
  data.frame(
    Date = as.Date(index(prices)),
    Open = as.numeric(Op(prices)),
    High = as.numeric(Hi(prices)),
    Low = as.numeric(Lo(prices)),
    Close = as.numeric(Cl(prices)),
    Volume = as.numeric(Vo(prices))
  ) |>
    filter(
      !is.na(Date),
      is.finite(Open),
      is.finite(High),
      is.finite(Low),
      is.finite(Close)
    ) |>
    arrange(Date)
}

# Convert daily prices into weekly/monthly OHLC data.
# Uses calendar grouping rather than xts conversion.

aggregate_prices <- function(data, period) {
  if (period == "Daily") {
    return(data)
  }
  
  if (period == "Weekly") {
    data$Group <- as.Date(
      cut(data$Date, breaks = "week", start.on.monday = TRUE)
    )
  } else {
    data$Group <- as.Date(format(data$Date, "%Y-%m-01"))
  }
  
  data |>
    group_by(Group) |>
    summarise(
      Date = max(Date),
      Open = first(Open),
      High = max(High),
      Low = min(Low),
      Close = last(Close),
      Volume = sum(Volume, na.rm = TRUE),
      .groups = "drop"
    ) |>
    select(-Group) |>
    arrange(Date)
}

# -------------------------------
# 2. Technical indicators
# -------------------------------

calculate_indicators <- function(data, short_n, long_n) {
  data <- arrange(data, Date)
  
  data$Short_MA <- as.numeric(
    SMA(data$Close, n = short_n)
  )
  
  data$Long_MA <- as.numeric(
    SMA(data$Close, n = long_n)
  )
  
  data$RSI <- as.numeric(
    RSI(data$Close, n = 14)
  )
  
  macd_values <- MACD(
    data$Close,
    nFast = 12,
    nSlow = 26,
    nSig = 9
  )
  
  data$MACD <- as.numeric(macd_values[, 1])
  data$MACD_Signal <- as.numeric(macd_values[, 2])
  
  data$Signal <- "Hold"
  
  valid <- !is.na(data$Short_MA) &
    !is.na(data$Long_MA)
  
  data$Signal[valid & data$Short_MA > data$Long_MA] <- "Buy"
  data$Signal[valid & data$Short_MA < data$Long_MA] <- "Sell"
  
  data$Previous_Signal <- dplyr::lag(data$Signal)
  
  data$Signal_Change <- valid &
    !is.na(data$Previous_Signal) &
    data$Signal != data$Previous_Signal
  
  data
}

# -------------------------------
# 3. Dashboard user interface
# -------------------------------

ui <- fluidPage(
  titlePanel("Stock Market Analysis | Nasreen Malik"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "symbol",
        "Stock Symbol",
        choices = c(
          "Apple (AAPL)" = "AAPL",
          "Microsoft (MSFT)" = "MSFT",
          "NVIDIA (NVDA)" = "NVDA",
          "Tesla (TSLA)" = "TSLA",
          "Amazon (AMZN)" = "AMZN"
        )
      ),
      
      dateRangeInput(
        "dates",
        "Date Range",
        start = "2022-01-01",
        end = Sys.Date(),
        min = "2015-01-01",
        max = Sys.Date()
      ),
      
      selectInput(
        "period",
        "Time Frame",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart_type",
        "Chart Type",
        choices = c("Line", "Candlestick", "Area")
      ),
      
      checkboxGroupInput(
        "indicators",
        "Technical Indicators",
        choices = c(
          "Moving Averages",
          "RSI",
          "MACD"
        ),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      
      numericInput(
        "short_ma",
        "Short Moving Average",
        value = 20,
        min = 2,
        max = 100
      ),
      
      numericInput(
        "long_ma",
        "Long Moving Average",
        value = 50,
        min = 3,
        max = 200
      ),
      
      helpText(
        "Trading signals are educational and are not financial advice."
      )
    ),
    
    mainPanel(
      h3("Historical Stock Prices"),
      plotOutput("price_chart", height = "430px"),
      
      conditionalPanel(
        condition = "input.indicators.includes('RSI')",
        h3("Relative Strength Index"),
        plotOutput("rsi_chart", height = "220px")
      ),
      
      conditionalPanel(
        condition = "input.indicators.includes('MACD')",
        h3("MACD Indicator"),
        plotOutput("macd_chart", height = "220px")
      ),
      
      h3("Latest Trading Signal"),
      verbatimTextOutput("latest_signal"),
      
      h3("Recent Trading Signal Changes"),
      tableOutput("signals_table")
    )
  )
)

# -------------------------------
# 4. Server and reactive analysis
# -------------------------------

server <- function(input, output, session) {
  
  stock_data <- reactive({
    result <- get_stock_prices(input$symbol)
    
    validate(
      need(!is.null(result), "Unable to download stock data.")
    )
    
    result
  })
  
  analysis_data <- reactive({
    req(input$dates, input$period)
    req(input$short_ma, input$long_ma)
    
    validate(
      need(
        input$short_ma < input$long_ma,
        "Short MA must be smaller than Long MA."
      )
    )
    
    data <- aggregate_prices(stock_data(), input$period)
    
    validate(
      need(
        nrow(data) > max(input$long_ma, 35),
        "Not enough historical observations."
      )
    )
    
    calculate_indicators(
      data,
      input$short_ma,
      input$long_ma
    )
  })
  
  visible_data <- reactive({
    data <- analysis_data()
    
    data |>
      filter(
        Date >= as.Date(input$dates[1]),
        Date <= as.Date(input$dates[2])
      )
  })
  
  # -------------------------------
  # 5. Stock price chart
  # -------------------------------
  
  output$price_chart <- renderPlot({
    data <- visible_data()
    
    validate(
      need(nrow(data) > 0, "No data in selected date range.")
    )
    
    p <- ggplot(data, aes(x = Date))
    
    if (input$chart_type == "Line") {
      p <- p +
        geom_line(aes(y = Close), color = "steelblue", linewidth = 0.8)
    } else if (input$chart_type == "Area") {
      p <- p +
        geom_area(aes(y = Close), fill = "skyblue", alpha = 0.5) +
        geom_line(aes(y = Close), color = "steelblue")
    } else {
      p <- p +
        geom_segment(
          aes(xend = Date, y = Low, yend = High),
          color = "gray40"
        ) +
        geom_rect(
          aes(
            xmin = Date - 0.35,
            xmax = Date + 0.35,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "gray30"
        ) +
        scale_fill_manual(
          values = c("TRUE" = "seagreen3", "FALSE" = "tomato"),
          guide = "none"
        )
    }
    
    if ("Moving Averages" %in% input$indicators) {
      p <- p +
        geom_line(
          aes(y = Short_MA, color = "Short MA"),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        geom_line(
          aes(y = Long_MA, color = "Long MA"),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        scale_color_manual(
          values = c(
            "Short MA" = "orange",
            "Long MA" = "purple"
          ),
          name = "Indicator"
        )
    }
    
    changes <- data |>
      filter(Signal_Change, Signal %in% c("Buy", "Sell"))
    
    if (nrow(changes) > 0) {
      p <- p +
        geom_point(
          data = changes,
          aes(y = Close, shape = Signal),
          size = 3,
          color = "black"
        ) +
        geom_text(
          data = changes,
          aes(y = Close, label = Signal),
          vjust = -1,
          size = 3,
          check_overlap = TRUE
        )
    }
    
    p +
      labs(
        title = paste(input$symbol, "-", input$period, "Prices"),
        x = "Date",
        y = "Price (USD)"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # -------------------------------
  # 6. RSI chart
  # -------------------------------
  
  output$rsi_chart <- renderPlot({
    data <- visible_data()
    
    validate(
      need(
        any(is.finite(data$RSI)),
        "RSI unavailable for this range."
      )
    )
    
    ggplot(data, aes(Date, RSI)) +
      geom_line(color = "darkorange", linewidth = 0.8) +
      geom_hline(yintercept = 70, linetype = "dashed") +
      geom_hline(yintercept = 30, linetype = "dashed") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "Date", y = "RSI") +
      theme_minimal()
  })
  
  # -------------------------------
  # 7. MACD chart
  # -------------------------------
  
  output$macd_chart <- renderPlot({
    data <- visible_data()
    
    validate(
      need(
        any(is.finite(data$MACD)),
        "MACD unavailable for this range."
      )
    )
    
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.8) +
      geom_line(
        aes(y = MACD_Signal, color = "Signal Line"),
        linewidth = 0.8
      ) +
      scale_color_manual(
        values = c(
          "MACD" = "blue",
          "Signal Line" = "red"
        ),
        name = NULL
      ) +
      labs(x = "Date", y = "MACD") +
      theme_minimal()
  })
  
  # -------------------------------
  # 8. Trading signal summary
  # -------------------------------
  
  output$latest_signal <- renderText({
    data <- visible_data()
    
    validate(
      need(nrow(data) > 0, "No data available.")
    )
    
    latest <- tail(data, 1)
    
    paste0(
      "Stock: ", input$symbol,
      "\nDate: ", format(latest$Date, "%Y-%m-%d"),
      "\nClosing Price: $", round(latest$Close, 2),
      "\nSignal: ", latest$Signal
    )
  })
  
  output$signals_table <- renderTable({
    data <- visible_data()
    
    data |>
      filter(Signal_Change, Signal %in% c("Buy", "Sell")) |>
      arrange(desc(Date)) |>
      head(10) |>
      transmute(
        Date = format(Date, "%Y-%m-%d"),
        Closing_Price = round(Close, 2),
        Signal = Signal
      )
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)
