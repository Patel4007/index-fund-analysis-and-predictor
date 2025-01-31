# Index Fund Analysis and Predictor

Index funds are investment funds that aim to replicate the performance of a specific market index, such as the S&P 500. They offer investors broad market exposure, low operating expenses, and low portfolio turnover. This project uses statistical analysis and machine learning to evaluate fund performance and predict future returns.

## Features

**Index Fund Performance Analysis**: Computes key metrics like rolling mean, standard deviation, skewness, and kurtosis.

**Predictive Analytics**: Implements a gradient boosting model to predict fund returns using historical data.

## Key Model Features

**Input Features**: Lagged returns (Lag1, Lag2, Lag5, etc.), rolling statistics (RollingMean_5, RollingSD_5), EMA values, and return differences.

**Training Strategy**: Uses caret::train() with XGBoost tuning (depth, learning rate, subsampling).

**Performance Metric**: Root Mean Square Error (RMSE) on test data.

## Installation & Setup

### Prerequisites

Ensure you have R installed along with the following libraries:

```R
install.packages(c("ggplot2", "shiny", "dplyr", "readr", "xgboost", "caret", "zoo", "TTR", "moments"))
```

### Running the Dashboard

Load your dataset (CSV format expected).

Run dashboard.R in an R environment (RStudio recommended).

View the interactive dashboard with fund analysis and predictions.

## Contribution

Contributions are welcome! Feel free to fork the repo, enhance models and submit pull requests.

## License

This project is licensed under the MIT License.


