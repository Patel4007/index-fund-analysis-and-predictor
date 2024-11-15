library(shiny)
library(shinydashboard)
library(plotly)
library(TTR)

source("Index_Funds_Analysis.R")

ui <- dashboardPage(
  dashboardHeader(title = "Index Funds Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Risk vs Return", tabName = "risk_return", icon = icon("chart-line")),
      menuItem("Cumulative Returns", tabName = "cumulative_returns", icon = icon("chart-area")),
      menuItem("Monthly Returns Histogram", tabName = "monthly_histogram", icon = icon("chart-bar"),
               menuSubItem("S&P 500", tabName = "sp500_histogram"),
               menuSubItem("NASDAQ", tabName = "nasdaq_histogram"),
               menuSubItem("Dow Jones", tabName = "dji_histogram")),
      menuItem("Annualized Returns", tabName = "annualized_returns", icon = icon("chart-pie")),
      menuItem("Box Plot of Returns", tabName = "box_plot", icon = icon("chart-box")),
      menuItem("Drawdown Plot", tabName = "drawdown_plot", icon = icon("chart-line")),
      menuItem("Actual vs Predicted Returns", tabName = "xgboost_plot", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "dashboard", h2("Financial Analysis Overview")),
      
      # Risk vs Return Plot
      tabItem(tabName = "risk_return", 
              plotlyOutput("riskReturnPlot"),
              h4("Provides an overview of the trade-off between risk (volatility) and return for each index. Useful for Investors to evaluate risk tolerance and make informed asset allocation decisions.")
      ),
      
      # Cumulative Returns Plot
      tabItem(tabName = "cumulative_returns", 
              plotlyOutput("cumulativeReturnsPlot"),
              h4("Displays cumulative daily returns for multiple indices over time. Beneficial for investors to compare the overall growth or decline of each index, visualize long-term trends and identify better-performing indices.")
      ),
      
      # Annualized Returns Plot
      tabItem(tabName = "annualized_returns", 
              plotOutput("annualizedReturnsPlot"),
              h4("Annualized returns give investors a way to measure how much an index or asset has returned per year over the entire observed period.")
      ),
      
      # Box Plot of Returns
      tabItem(tabName = "box_plot",  
              h3("Box Plot of Daily Returns"),  
              plotOutput("boxPlotReturns"),
              h4("Provides insight into index risk by showing daily return variability. Helps investors understand volatility, assess outliers, and guide asset allocation decisions.")
      ),
      
      # Drawdown Plot
      tabItem(tabName = "drawdown_plot", 
              plotOutput("drawdownPlot"),
              h4("Visualizes maximum drawdowns for each index. Through this, investors understand significant declines from peak values and assess potential losses during market downturns.")
      ),
      
      # Separate histogram tabs for each index
      tabItem(tabName = "sp500_histogram", plotOutput("sp500HistogramPlot")),
      tabItem(tabName = "nasdaq_histogram", plotOutput("nasdaqHistogramPlot")),
      tabItem(tabName = "dji_histogram", plotOutput("djiHistogramPlot")),
      
      # XGBoost Prediction tab
      tabItem(tabName = "xgboost_plot",
              h3("XGBoost Prediction for 2025"),
              textOutput("predictedValue2025"),  
              plotOutput("xgboostPredictionPlot")
      )
    )
  )
)

server <- function(input, output) {
  
  get_data <- function(symbols, start_date, end_date) {
    index_data_list <- lapply(symbols, function(symbol) {
      tryCatch({
        getSymbols(symbol, src = "yahoo", auto.assign = FALSE, from = start_date, to = end_date)
      }, error = function(e) {
        message(sprintf("Failed to retrieve data for %s: %s", as.character(symbol), e$message))
        return(NULL)
      })
    })
    
    names(index_data_list) <- symbols
    return(index_data_list)
  }
  
  # Fetch data
  start_date <- "2010-01-01"
  end_date <- Sys.Date()
  symbols <- c("^GSPC", "^IXIC", "^DJI", "EEM", "XOP")
  index_data <- get_data(symbols, start_date, end_date)
  
  daily_returns <- list(
    "^GSPC" = dailyReturn(Ad(index_data[["^GSPC"]]), na.rm = TRUE),
    "^IXIC" = dailyReturn(Ad(index_data[["^IXIC"]]), na.rm = TRUE),
    "^DJI" = dailyReturn(Ad(index_data[["^DJI"]]), na.rm = TRUE),
    "EEM" = dailyReturn(Ad(index_data[["EEM"]]), na.rm = TRUE),
    "XOP" = dailyReturn(Ad(index_data[["XOP"]]), na.rm = TRUE)
  )

  returns_result <- calculate_returns(index_data[["^GSPC"]])
  returns_result_ixic <- calculate_returns(index_data[["^IXIC"]])
  returns_result_dji <- calculate_returns(index_data[["^DJI"]])
  
  output$cumulativeReturnsDescription <- renderText({
    "This plot shows the cumulative daily returns for multiple indices over the observed period. It allows investors to compare the overall growth or decline of each index, helping them visualize long-term trends and identify which indices performed better over time."
  })
  
  # Risk vs Return Description
  output$riskReturnDescription <- renderText({
    "This plot provides an overview of the trade-off between risk (volatility) and return for each index. Investors can use this to evaluate their risk tolerance and make informed decisions on asset allocation by choosing indices that match their desired risk-return profile."
  })
  
  # Annualized Returns Description
  output$annualizedReturnsDescription <- renderText({
    "This plot highlights the annualized returns for each index, giving investors a clear understanding of the average yearly performance. Investors can use this information to assess long-term growth potential and compare how different indices stack up over time."
  })
  
  # Drawdown Plot Description
  output$drawdownDescription <- renderText({
    "This visualization displays the maximum percentage declines from peak values (drawdowns) for each index. Investors can use this to understand the risk of significant losses and evaluate the resilience of each index during market downturns."
  })
  
  # Box Plot Description (already present)
  output$boxPlotDescription <- renderText({
    "This box plot provides insight into the risk associated with each index by showing the variability in daily returns. Investors can assess potential losses from outliers and use this information to better understand the volatility of each index, guiding decisions on asset allocation and risk management strategies."
  })
  
  # Risk vs Return plot
  output$riskReturnPlot <- renderPlotly({
    plot_risk_return_multiple(returns_dm, risk_metrics)
  })
  
  # Cumulative Returns plot
  output$cumulativeReturnsPlot <- renderPlotly({
    plot_cumulative_returns_multiple(index_data)
  })
  
  # Annualized Returns plot
  output$annualizedReturnsPlot <- renderPlot({
    plot_annualized_returns(performance_results)
  })
  
  # Box Plot of Returns
  output$boxPlotReturns <- renderPlot({
    plot_boxplot_risk_vs_reward(daily_returns)
  })
  
  # Drawdown Plot
  output$drawdownPlot <- renderPlot({
    drawdown_plot_multiple <- chart.Drawdown(combined_returns, main = "Drawdown Plot for Multiple Indices", plot.engine = "ggplot2")
    print(drawdown_plot_multiple)
  })
  
  # S&P 500 Monthly Histogram
  output$sp500HistogramPlot <- renderPlot({
    monthly_returns_vector <- as.numeric(returns_result$Monthly)
    plot_histogram_monthly(monthly_returns_vector) +
      labs(title = "Monthly Returns Histogram - S&P 500")
  })
  
  # NASDAQ Monthly Histogram
  output$nasdaqHistogramPlot <- renderPlot({
    monthly_returns_vector_ixic <- as.numeric(returns_result_ixic$Monthly)
    plot_histogram_monthly(monthly_returns_vector_ixic) +
      labs(title = "Monthly Returns Histogram - NASDAQ")
  })
  
  # Dow Jones Monthly Histogram
  output$djiHistogramPlot <- renderPlot({
    monthly_returns_vector_dji <- as.numeric(returns_result_dji$Monthly)
    plot_histogram_monthly(monthly_returns_vector_dji) +
      labs(title = "Monthly Returns Histogram - Dow Jones")
  })
  
  # Prediction for 2025 using XGBoost
  output$predictedValue2025 <- renderText({
    last_known_returns <- tail(returns_dm, 10)
    
    last_known_returns <- last_known_returns %>% ungroup()
    
    last_known_scaled <- scale_features(last_known_returns)
    
    missing_cols <- setdiff(names(train_data %>% select(-Index, -Date, where(is.numeric))), names(last_known_scaled))
    if (length(missing_cols) > 0) {
      cat("Missing columns in last known data:", missing_cols, "\n")
    }
    
    last_row_scaled <- last_known_scaled %>% tail(1)
    
    if (nrow(last_row_scaled) > 0) {
      predicted_return_2025 <- predict(xgb_model, newdata = last_row_scaled)
      print(paste("Predicted Cumulative Return in the first week of January 2025:", predicted_return_2025))
    } else {
      print("Error: last_row_scaled is empty.")
    }
  })
  
  output$xgboostPredictionPlot <- renderPlot({
    returns_dm <- returns_dm %>%
      group_by(Index) %>%
      mutate(Lag1 = lag(Returns, 1),
             Lag2 = lag(Returns, 2),
             Lag3 = lag(Returns, 3),
             Lag5 = lag(Returns, 5),
             Lag10 = lag(Returns, 10),
             RollingMean_5 = rollmean(Returns, 5, fill = NA),
             RollingMean_10 = rollmean(Returns, 10, fill = NA),
             RollingSD_5 = rollapply(Returns, 5, sd, fill = NA),
             RollingSD_10 = rollapply(Returns, 10, sd, fill = NA),
             RollingSkew_5 = rollapply(Returns, 5, moments::skewness, fill = NA),
             RollingKurt_5 = rollapply(Returns, 5, moments::kurtosis, fill = NA),
             EMA_5 = EMA(Returns, n = 5),
             EMA_10 = EMA(Returns, n = 10),
             Diff_Lag1_Lag5 = Lag1 - Lag5) %>%
      na.omit()
    
    set.seed(123)
    trainIndex <- createDataPartition(returns_dm$Returns, p = 0.8, list = FALSE, times = 1)
    train_data <- returns_dm[trainIndex, ]
    test_data <- returns_dm[-trainIndex, ]
    
    scale_features <- function(data) {
      data %>% mutate(across(where(is.numeric), scale))
    }
    train_data <- scale_features(train_data)
    test_data <- scale_features(test_data)
    
    train_data <- train_data %>% ungroup()
    test_data <- test_data %>% ungroup()
    
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-Index, -Date)), label = train_data$Returns)
    test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-Index, -Date)), label = test_data$Returns)
    
    xgb_grid <- expand.grid(
      nrounds = c(50),  
      max_depth = c(15),
      eta = c(0.05), 
      gamma = 0,
      colsample_bytree = 0.8, 
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
    
    xgb_model <- train(
      Returns ~ Lag1 + Lag2 + Lag3 + Lag5 + Lag10 + RollingMean_5 + RollingMean_10 + 
        RollingSD_5 + RollingSD_10 + RollingSkew_5 + RollingKurt_5 + 
        EMA_5 + EMA_10 + Diff_Lag1_Lag5, 
      data = train_data, 
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid
    )
    
    predictions <- predict(xgb_model, newdata = test_data)
    
    rmse <- sqrt(mean((predictions - test_data$Returns)^2))
    print(paste("RMSE on test set:", rmse))
    
    ggplot() +
      geom_line(aes(x = test_data$Date, y = test_data$Returns, color = "Actual"), size = 1) +
      geom_line(aes(x = test_data$Date, y = predictions, color = "Predicted"), size = 1) +
      labs(title = "Predicted vs Actual Returns", x = "Date", y = "Returns") +
      scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()
    
  })
}

shinyApp(ui = ui, server = server)