library(readr)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(randomForest)
library(tibble)


# 1. LOAD & PREP DATA

file_path <- "C:\\Users\\Yuvan Rahul\\Downloads\\EUcoviddata.csv"
df <- read_csv(file_path)

df <- df %>% 
  mutate(date = dmy(dateRep)) %>% 
  arrange(date)


# 2. AGGREGATE ALL COUNTRIES TO DAILY TOTALS

df_all <- df %>%
  group_by(date) %>%
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)


# 3. LOG TRANSFORMATION

df_all$log_cases <- log(df_all$cases + 1)

ts_data <- ts(df_all$log_cases, frequency = 7)
n <- length(ts_data)


# 4. TRAIN/TEST SPLIT

h <- 10
train <- ts_data[1:(n - h)]
test_actual <- df_all$cases[(n - h + 1):n]  


# 5. FIT MODELS (LOGGED SERIES)

fit_naive <- naive(train, h = h)
fit_arima <- auto.arima(train)
fit_ets   <- ets(train)
fit_tbats <- tbats(train)

fc_arima  <- forecast(fit_arima, h = h)
fc_ets    <- forecast(fit_ets, h = h)
fc_tbats  <- forecast(fit_tbats, h = h)

pred_naive <- exp(fit_naive$mean) - 1
pred_arima <- exp(fc_arima$mean) - 1
pred_ets   <- exp(fc_ets$mean) - 1
pred_tbats <- exp(fc_tbats$mean) - 1


# 6. RANDOM FOREST

lags <- 7
rf_data <- data.frame(cases = df_all$cases)

for (i in 1:lags) {
  rf_data[[paste0("lag", i)]] <- dplyr::lag(rf_data$cases, i)
}
rf_data <- rf_data %>% filter(complete.cases(.))

rf_train <- rf_data[1:(n - h - lags), ]
rf_test  <- rf_data[(n - h - lags + 1):(n - lags), ]

rf_model <- randomForest(cases ~ ., data = rf_train)
pred_rf <- predict(rf_model, newdata = rf_test)

# 7. VALIDATION METRICS

actual <- test_actual

rmse <- function(a, p) sqrt(mean((a - p)^2))
mae  <- function(a, p) mean(abs(a - p))
mape <- function(a, p) mean(abs((a - p) / pmax(a, 1))) * 100
r2   <- function(a, p) {
  ss_res <- sum((a - p)^2)
  ss_tot <- sum((a - mean(a))^2)
  1 - ss_res / ss_tot
}

metrics <- data.frame(
  Model = c("Naive (log)", "ARIMA (log)", "ETS (log)", "TBATS (log)", "Random Forest"),
  RMSE = c(rmse(actual, pred_naive),
           rmse(actual, pred_arima),
           rmse(actual, pred_ets),
           rmse(actual, pred_tbats),
           rmse(actual, pred_rf)),
  MAE = c(mae(actual, pred_naive),
          mae(actual, pred_arima),
          mae(actual, pred_ets),
          mae(actual, pred_tbats),
          mae(actual, pred_rf)),
  MAPE = c(mape(actual, pred_naive),
           mape(actual, pred_arima),
           mape(actual, pred_ets),
           mape(actual, pred_tbats),
           mape(actual, pred_rf)),
  R2 = c(r2(actual, pred_naive),
         r2(actual, pred_arima),
         r2(actual, pred_ets),
         r2(actual, pred_tbats),
         r2(actual, pred_rf))
)

print("\n VALIDATION METRICS (ALL MODELS) ")
print(metrics)


# 8. FULL TIME SERIES PLOT WITH DATE AXIS

ggplot(df_all, aes(x = date, y = cases)) +
  geom_line(color = "black") +
  ggtitle("Daily COVID-19 Cases – All Countries (Raw Data)") +
  xlab("Date") + ylab("Cases")   +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  theme_minimal()


# 9. 30-DAY FORECAST WITH TBATS

final_fit <- tbats(ts_data)
final_fc  <- forecast(final_fit, h = 30)

future_dates <- seq(max(df_all$date) + 1, by = "day", length.out = 30)

fc_df <- data.frame(
  date = future_dates,
  mean = exp(final_fc$mean) - 1,
  lower = exp(final_fc$lower[, 2]) - 1,
  upper = exp(final_fc$upper[, 2]) - 1
)


hist_df <- df_all %>% select(date, cases)

ggplot() +
  geom_line(data = hist_df, aes(x = date, y = cases), color = "black") +
  geom_ribbon(data = fc_df, aes(x = date, ymin = lower, ymax = upper),
              fill = "blue", alpha = 0.25) +
  geom_line(data = fc_df, aes(x = date, y = mean), color = "blue", size = 1) +
  ggtitle("Final 30-Day Forecast – All Countries Combined (TBATS)") +
  xlab("Date") + ylab("Cases") +
  theme_minimal()


# 10. COMPARISON PLOT: ACTUAL vs ALL MODELS

test_dates <- df_all$date[(n - h + 1):n]

comparison_df <- data.frame(
  Date = rep(test_dates, 5),
  Cases = c(actual, pred_arima, pred_ets, pred_tbats, pred_rf),
  Model = rep(c("Actual", "ARIMA", "ETS", "TBATS", "Random Forest"),
              each = length(actual))
)

ggplot(comparison_df, aes(x = Date, y = Cases, color = Model)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  ggtitle("Actual vs Predicted COVID-19 Cases (Test Set)") +
  xlab("Date") + ylab("Daily Cases") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Actual" = "black",
    "ARIMA" = "salmon",
    "ETS" = "lightblue",
    "TBATS" = "purple",
    "Random Forest" = "lightgreen"
  ))


# 11. Results Table

test_dates <- df_all$date[(n - h + 1):n]

combined_table <- data.frame(
  Date = test_dates,
  Actual = actual,
  Naive = round(pred_naive),
  ARIMA = round(pred_arima),
  ETS = round(pred_ets),
  TBATS = round(pred_tbats),
  Random_Forest = round(pred_rf)
)


