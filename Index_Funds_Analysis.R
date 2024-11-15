library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(xts)
library(caret)
library(xgboost)

# Function to get index data
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

start_date <- "2010-01-01"
end_date <- Sys.Date()
symbols <- c("^GSPC", "^IXIC", "^DJI", "EEM", "XOP") 
index_data <- get_data(symbols, start_date, end_date)

print(index_data)

# Function to calculate different types of returns
calculate_returns <- function(data) {
  daily_returns <- dailyReturn(Ad(data), na.rm = TRUE)
  weekly_returns <- periodReturn(Ad(data), period = "weekly", type = "arithmetic", na.rm = TRUE)
  monthly_returns <- periodReturn(Ad(data), period = "monthly", type = "arithmetic", na.rm = TRUE)
  yearly_returns <- periodReturn(Ad(data), period = "yearly", type = "arithmetic", na.rm = TRUE)
  
  return(list(
    Daily = daily_returns,
    Weekly = weekly_returns,
    Monthly = monthly_returns,
    Yearly = yearly_returns
  ))
}

returns_to_df <- function(returns_list) {
  df <- do.call(rbind, lapply(names(returns_list), function(index) {
    data.frame(
      Date = index(returns_list[[index]]),
      Returns = coredata(returns_list[[index]]),  
      Index = index
    )
  }))
  return(df)
}

performance_analysis <- function(returns_df) {
  performance_metrics <- returns_df %>%
    group_by(Index) %>%
    do({
      returns_xts <- xts(.$Returns, order.by = .$Date)
      
      data.frame(
        Annualized_Return = Return.annualized(returns_xts, scale = 252) * 100,
        Sharpe_Ratio = SharpeRatio.annualized(returns_xts, Rf = 0),
        Total_Return = Return.cumulative(returns_xts) * 100
      )
    })
  
  return(performance_metrics)
}




# Function to calculate risk metrics
risk_analysis <- function(returns_dm) {
  risk_metrics <- returns_dm %>%
    group_by(Index) %>%
    do({
      returns_xts <- xts(.$Returns, order.by = .$Date)
      
      annualized_return <- Return.annualized(returns_xts, scale = 252) * 100  
      volatility <- sd(.$Returns, na.rm = TRUE) * sqrt(252)  
      sharpe_ratio <- SharpeRatio.annualized(returns_xts, Rf = 0)
      max_drawdown <- maxDrawdown(returns_xts)
      var_95 <- quantile(returns_xts, 0.05, na.rm = TRUE)  
      var_99 <- quantile(returns_xts, 0.01, na.rm = TRUE)  
      
      data.frame(
        Index = unique(.$Index),
        Annualized_Return = as.numeric(annualized_return),  
        Volatility = volatility,
        Sharpe_Ratio = as.numeric(sharpe_ratio),
        Max_Drawdown = as.numeric(max_drawdown),
        VaR_95 = as.numeric(var_95),
        VaR_99 = as.numeric(var_99)
      )
    })
  
  return(risk_metrics)
}


# Box plot for annualized returns
plot_annualized_returns <- function(performance_data) {
  ggplot(performance_data, aes(x = Index, y = Annualized_Return, fill = Index)) +
    geom_bar(stat = "identity") +
    labs(title = "Annualized Returns by Index", x = "Index", y = "Annualized Return (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title
}

create_boxplot_data <- function(returns_list) {
  combined_returns <- data.frame(
    Returns = c(as.numeric(returns_list[["^GSPC"]]), 
                as.numeric(returns_list[["^IXIC"]]), 
                as.numeric(returns_list[["^DJI"]]),
                as.numeric(returns_list[["EEM"]]),
                as.numeric(returns_list[["XOP"]])),
    Index = factor(rep(c("S&P 500", "NASDAQ", "Dow Jones", "Emerging Markets", "Oil and Gas Companies"), 
                       times = c(length(returns_list[["^GSPC"]]), 
                                 length(returns_list[["^IXIC"]]), 
                                 length(returns_list[["^DJI"]]),
                                 length(returns_list[["EEM"]]),
                                 length(returns_list[["XOP"]]))))
  )
  return(combined_returns)
}

# Box plot for risk vs reward analysis
plot_boxplot_risk_vs_reward <- function(returns_list) {
  boxplot_data <- create_boxplot_data(returns_list)
  
  ggplot(boxplot_data, aes(x = Index, y = Returns, fill = Index)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16) +
    labs(title = "Box Plot of Daily Returns by Index", x = "Index", y = "Daily Returns") +
    theme_minimal()
}



# Function to plot risk vs return for multiple indices
plot_risk_return_multiple <- function(returns_df, risk_metrics) {
  
  risk_return_df <- returns_df %>%
    group_by(Index) %>%
    summarize(
      Mean_Return = mean(Returns, na.rm = TRUE) * 100  
    )
  
  combined_df <- merge(risk_return_df, risk_metrics, by = "Index")
  
  plot <- ggplot(combined_df, aes(x = Volatility, y = Annualized_Return, color = Index, 
                                  text = paste("Index:", Index, 
                                               "<br>Annualized Return:", round(Annualized_Return, 2), "%",
                                               "<br>Volatility:", round(Volatility, 2), 
                                               "<br>Sharpe Ratio:", round(Sharpe_Ratio, 2), 
                                               "<br>Max Drawdown:", round(Max_Drawdown * 100, 2), "%",
                                               "<br>VaR 95%:", round(VaR_95 * 100, 2), "%",
                                               "<br>VaR 99%:", round(VaR_99 * 100, 2), "%"))) +
    geom_point(size = 3) +  
    labs(title = "Risk vs. Return Analysis (Multiple Indices)", 
         x = "Volatility (Risk)", 
         y = "Annualized Return (%)") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(ggplotly(plot, tooltip = "text"))
}


# Function to create cumulative return line plot for multiple indices
plot_cumulative_returns_multiple <- function(index_data) {
  
  cumulative_df <- data.frame(Date = NULL, Cumulative_Returns = NULL, Index = NULL)
  
  for (symbol in names(index_data)) {
    daily_returns <- dailyReturn(Ad(index_data[[symbol]]), na.rm = TRUE)  # Calculate daily returns
    
    
    cumulative_returns <- cumprod(1 + daily_returns) - 1
    
    temp_df <- data.frame(Date = index(daily_returns),
                          Cumulative_Returns = as.numeric(cumulative_returns),
                          Index = symbol)
    
    cumulative_df <- rbind(cumulative_df, temp_df)
  }
  
  line_plot <- ggplot(cumulative_df, aes(x = Date, y = Cumulative_Returns, color = Index)) +
    geom_line(size = 1.2) +
    labs(title = "Cumulative Daily Returns for S&P 500, NASDAQ, and Dow Jones", 
         x = "Date", y = "Cumulative Return (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12))
  
  interactive_plot <- ggplotly(line_plot, tooltip = c("Date", "Cumulative_Returns", "Index"))
  
  return(interactive_plot)
}




# Function to create histogram of daily returns

plot_histogram_monthly <- function(monthly_returns) {

  histogram_data <- data.frame(Monthly_Returns = monthly_returns)
  
  histogram_plot <- ggplot(histogram_data, aes(x = Monthly_Returns)) +
    geom_histogram(binwidth = 0.01, fill = "#3357FF", color = "black", alpha = 0.7, aes(y = ..count..)) +
    labs(title = "Histogram of Monthly Returns", x = "Monthly Returns", y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12))
  
  histogram_build <- ggplot_build(histogram_plot)
  
  counts <- histogram_build$data[[1]]$count
  breaks <- histogram_build$data[[1]]$x
  
  mids <- (breaks[-length(breaks)] + breaks[-1]) / 2
  
  if (length(counts) != length(mids)) {
    counts <- counts[-length(counts)]  
  }
  
  label_data <- data.frame(mids = mids, counts = counts)
  
  histogram_with_labels <- histogram_plot +
    geom_text(data = label_data, aes(x = mids, y = counts, label = counts), 
              vjust = -0.5, size = 3.5, color = "black")  
  
  return(histogram_with_labels)
}


returns_result <- list(
  "^GSPC" = dailyReturn(Ad(index_data[["^GSPC"]]), na.rm = TRUE),
  "^IXIC" = dailyReturn(Ad(index_data[["^IXIC"]]), na.rm = TRUE),
  "^DJI" = dailyReturn(Ad(index_data[["^DJI"]]), na.rm = TRUE),
  "EEM" = dailyReturn(Ad(index_data[["EEM"]]), na.rm = TRUE),
  "XOP" = dailyReturn(Ad(index_data[["XOP"]]), na.rm = TRUE)
)

performance_returns <- lapply(index_data, calculate_returns)

daily_returns_list <- lapply(performance_returns, function(x) x$Daily)

returns_df <- returns_to_df(daily_returns_list)


returns_df <- returns_df %>%
  rename(Returns = daily.returns)


print(head(returns_df))
str(returns_df)

performance_metrics <- performance_analysis(returns_df)
print(performance_metrics)

arranged_returns <- returns_df %>% arrange(desc(Returns))

filtered_returns <- returns_df %>% filter(Index == "^GSPC")

normalized_returns <- returns_df %>% mutate(Normalized_Return = scale(Returns))

summary_stats <- returns_df %>% group_by(Index) %>% summarize(
  Avg_Return = mean(Returns, na.rm = TRUE),
  Std_Dev = sd(Returns, na.rm = TRUE)
)

pie_data <- performance_analysis(returns_df)

pie_chart <- ggplot(pie_data, aes(x = "", y = Sharpe_Ratio, fill = Index)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  labs(title = "Sharpe Ratio by Index", x = "", y = "") +
  theme_minimal()

print(pie_chart)

returns_dm <- returns_to_df(returns_result)

returns_dm <- returns_dm %>%
  rename(Returns = daily.returns)


print(returns_dm %>% filter(Index == "EEM"))
print(returns_dm %>% filter(Index == "XOP"))


print(returns_dm)

performance_metrics <- performance_analysis(returns_dm)
risk_metrics <- risk_analysis(returns_dm)

print(risk_metrics)

daily_returns_xts <- returns_result

combined_returns <- cbind(
  S_P500 = returns_result[["^GSPC"]],
  NASDAQ = returns_result[["^IXIC"]],
  DOW_JONES = returns_result[["^DJI"]],
  SPDR_SP_Oil_and_Gas = returns_result[["XOP"]],
  iShares_MSCI = returns_result[["EEM"]]
)

drawdown_plot_multiple <- chart.Drawdown(combined_returns, main = "Drawdown Plot for Multiple Indices", plot.engine = "ggplot2")
print(drawdown_plot_multiple)

boxplot_risk_reward <- plot_boxplot_risk_vs_reward(returns_result)
print(boxplot_risk_reward)

print(returns_result)

cumulative_return_plot <- plot_cumulative_returns_multiple(index_data)
print(cumulative_return_plot)

risk_return_plot <- plot_risk_return_multiple(returns_dm, risk_metrics)
print(risk_return_plot)

performance_results <- performance_analysis(returns_dm)
print(performance_results)

annualized_returns_plot <- plot_annualized_returns(performance_results)
print(annualized_returns_plot)

monthly_returns_vector <- as.numeric(returns_result$Monthly)  
histogram_plot_monthly <- plot_histogram_monthly(monthly_returns_vector)
print(histogram_plot_monthly)

print(range(as.Date(index(returns_result$Daily))))
