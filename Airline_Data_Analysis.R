library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
library(timetk)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)

raw <- fread('AirPassengers.csv') 

raw %>% glimpse()
raw %>% skim()

interactive<-FALSE
colnames(raw) <- c('Date','Count')

raw$Date <- lubridate::ym(raw$Date) #change to date format

raw <- raw %>%  
  group_by(Date) %>%
  summarise(Count = sum(Count)) 

raw %>% 
  plot_time_series(Date,Count)

splits <- initial_time_split(raw, prop = 0.9)

#model1
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Count ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
      data = training(splits))

# Model 2: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Count ~ Date, data = training(splits))

# Model 3: prophet ----
model_fit_prophet <- prophet_reg(seasonality_yearly = T) %>%
  set_engine(engine = "prophet") %>%
  fit(Count ~ Date, data = training(splits))

#Step 3 - Add fitted models to a Model Table.----

models_tbl <- modeltime_table(
  
  
  model_fit_ets,
  model_fit_prophet
  
)

models_tbl

#Step 4 - Calibrate the model to a testing set.----

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl



#Step 5 - Testing Set Forecast & Accuracy Evaluation----
#5A - Visualizing the Forecast Test----
calibration_tbl %>%
  filter(.model_id == 1) %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = raw
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


#5B - Accuracy Metrics----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

#Step 6 - Refit to Full Dataset & Forecast Forward----

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = raw)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = raw) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )














