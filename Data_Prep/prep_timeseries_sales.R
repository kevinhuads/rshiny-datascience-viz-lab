# Data preparation for the time series section

# Required packages
library(tibble)   # as_tibble
library(prophet)  # prophet, add_seasonality, fit.prophet, make_future_dataframe
library(lubridate) # yday, year

sales_train = read.csv('Data_Prep/Rawdata/sales_train.csv', stringsAsFactors = FALSE) # raw daily item–store sales
sales_train$date = as.Date(sales_train$date)                               # convert date column to Date

k_store = 3  # number of stores kept for the demo
k_item  = 5  # number of items kept for the demo

# keep only the first k_store stores and first k_item items
sales_train = sales_train[(sales_train$store %in% 1:k_store) &
                            (sales_train$item  %in% 1:k_item), ]

# grid of Prophet seasonal configurations to test
sales_prophgrid <- expand.grid(
  yearly_list  = c('none', 'additive', 'multiplicative'),
  monthly_list = c('none', 'additive', 'multiplicative'),
  weekly_list  = c('none', 'additive', 'multiplicative')
)

# grid of (item, store) combinations
sales_itemgrid <- expand.grid(
  item_list  = 1:k_item,
  store_list = 1:k_store
)

# fit Prophet models for each item–store pair and each seasonal configuration
sales_prophet = lapply(1:nrow(sales_itemgrid), function(k) {
  item  = sales_itemgrid$item_list[k]
  store = sales_itemgrid$store_list[k]
  
  db = as_tibble(sales_train[sales_train$item == item &
                               sales_train$store == store,
                             c('sales', 'date')])
  db = db[db$date < "2017-01-01", ]  # training period only
  
  stats = data.frame(y = db$sales, ds = db$date)
  stats = aggregate(stats$y, by = list(stats$ds), FUN = sum)
  colnames(stats) <- c("ds", "y")
  
  lapply(1:nrow(sales_prophgrid), function(i) {
    
    list_final = list()
    
    year_  = as.character(sales_prophgrid$yearly_list[i])
    month_ = as.character(sales_prophgrid$monthly_list[i])
    week_  = as.character(sales_prophgrid$weekly_list[i])
    
    # base model with multiplicative seasonality mode
    model_prophet = prophet(
      daily.seasonality  = FALSE,
      weekly.seasonality = FALSE,
      yearly.seasonality = FALSE,
      seasonality.mode   = 'multiplicative'
    )
    
    # add custom seasonalities according to the grid
    if (year_  != 'none')
      model_prophet = add_seasonality(
        model_prophet, name = 'yearly', period = 365.25,
        fourier.order = 10, mode = year_
      )
    
    if (month_ != 'none')
      model_prophet = add_seasonality(
        model_prophet, name = 'monthly', period = 30.5,
        fourier.order = 10, mode = month_
      )
    
    if (week_  != 'none')
      model_prophet = add_seasonality(
        model_prophet, name = 'weekly', period = 7,
        fourier.order = 10, mode = week_
      )
    
    model_prophet = fit.prophet(model_prophet, stats)
    
    future   = make_future_dataframe(model_prophet, periods = 365) # forecast one year ahead
    forecast = predict(model_prophet, future)
    
    # store trend and yhat for error computation
    list_final[['trend']] = forecast[, c('trend', 'yhat')]
    
    # weekly seasonal pattern (mean by weekday)
    if (week_ != 'none') {
      week = aggregate(forecast$weekly,
                       by  = list(weekdays(forecast$ds)),
                       FUN = mean)
      colnames(week) = c('day', 'value')
      week$day = factor(
        week$day,
        levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                   "Friday", "Saturday", "Sunday")
      )
      week = week[order(week$day), ]
      list_final[['week']] = week
    }
    
    # within-month seasonal pattern (mean by day-of-month)
    if (month_ != 'none') {
      month = aggregate(
        forecast$monthly,
        by  = list(as.numeric(substr(as.Date(forecast$ds), 9, 10))),
        FUN = mean
      )
      colnames(month) = c('day', 'value')
      list_final[['month']] = month
    }
    
    # within-year seasonal pattern (mean by day-of-year, adjusted for leap year)
    if (year_ != 'none') {
      yday_ = yday(forecast$ds)
      idx   = which((as.Date(forecast$ds) >= "2016-02-29") &
                      (year(forecast$ds) == 2016))
      yday_[idx] = yday_[idx] - 1
      year = aggregate(forecast$yearly, by = list(yday_), FUN = mean)
      colnames(year) = c('day', 'value')
      list_final[['year']] = year
    }
    
    return(list_final)
  })
})

# compute MAE and RMSE on the 2017 hold-out period for each model/grid combo
sales_prophet_err = lapply(c('MAE', 'RMSE'), function(N) {
  lapply(1:nrow(sales_itemgrid), function(k) {
    item  = sales_itemgrid$item_list[k]
    store = sales_itemgrid$store_list[k]
    
    db = as_tibble(sales_train[sales_train$item == item &
                                 sales_train$store == store,
                               c('sales', 'date')])
    
    z = sapply(1:nrow(sales_prophgrid), function(i) {
      db$pred  = sales_prophet[[k]][[i]]$trend$yhat
      db_test  = db[db$date >= "2017-01-01", ]
      
      if (N == 'MAE') {
        y = mean(abs(db_test$sales - db_test$pred))
      } else {
        y = sqrt(mean((db_test$sales - db_test$pred)^2, na.rm = TRUE))
      }
      return(y)
    })
    return(z)
  })
})
names(sales_prophet_err) = c('MAE', 'RMSE')

# save all objects whose name contains "sales"
save(list = ls(pattern = 'sales'), file = 'Application/Saved/sales.RData')
