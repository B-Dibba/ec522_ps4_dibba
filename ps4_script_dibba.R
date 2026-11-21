
#install.packages("BVAR")
#install.packages("urca")
#install.packages("tidyverse")
#install.packages("fpp3")

#library(tidyverse)
library(fpp3)
library(BVAR)
library(urca)
library(readr) 

#Part 2 – A Business Forecasting Horse Race: ARIMA vs. ETS
#2.1a. Read the file and build a tsibble. We end the sample in December 2019 so that the pandemic, which caused unusual swings in this series, does not dominate the results.

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)


#2.1b. Plot sales with autoplot(), and make a season plot of \(\log(\text{sales})\) with gg_season().
#Add a title and axis labels to each. In a few sentences: describe the trend and the seasonal pattern. Which months are high and which are low?

liquor |>
  autoplot(sales) +
  labs(
    title = "Monthly U.S. Retail Sales: Beer, Wine & Liquor Stores",
    x     = "Month",
    y     = "Sales (millions of dollars)"
  )

# Season plot of log(sales) with gg_season()
liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Plot of Log Liquor Sales (1992–2019)",
    x     = "Month",
    y     = "log(Sales)"
  )



# Do the seasonal swings grow with the level of the series, and what does that suggest about using logs?


#2.2. Fit Both Models
#2.2a. Fit an ARIMA and an ETS model to the log of sales over the entire sample period, from January 1992 to December 2019, letting fable choose the specification of each:
  
  liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

#Which specification was chosen for each model? In words: what does the chosen ARIMA say about how the series was differenced? What do the three letters of the chosen ETS say about its error, trend, and seasonal components?

#2.2b. Check the residuals of the ARIMA model with gg_tsresiduals() and a Ljung–Box test at lag 24. Fill in dof with the number of estimated AR and MA coefficients (\(p + q + P + Q\)) in the model chosen in 2.2a. Do the residuals look like white noise? Failing this test doesn’t tell us that a model will forecast badly, only that there may be structure left to capture. The horse race below will tell us how well the model actually forecasts.

# 2.2b – Residual diagnostics for the ARIMA model

# Residual plots (time plot, ACF, histogram)
liquor_fit |>
  select(arima) |>
  gg_tsresiduals()

# Ljung-Box test at lag 24
# dof = p + q + P + Q = 1 + 2 + 2 + 1 = 6
liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

#2.3. The Horse Race
#2.3a. Add the following to your script. It runs an expanding-window TS-CV with a first training sample of 120 months (10 years), and fits three models to each training sample: the two from Part 2.2 above and a seasonal random walk benchmark. 

#liquor_cv <- liquor |>
  #stretch_tsibble(.init = 120, .step = 1) |>
  #filter(.id < max(.id))

#liquor_cv_fit <- liquor_cv |>
  #model(snaive = SNAIVE(sales),
        #ets    = ETS(log(sales)),
        #arima  = ARIMA(log(sales)))

#saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

#How many different forecast origins will be considered in this TS-CV?

#2.3b. Produce the one-month-ahead forecasts and compute their accuracy.

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

#What are the dates of the first and last forecasts? 

#Report the accuracy results in a table. In your write-up, you can produce a nicely formatted table by piping the results into the kable() function from the knitr package: add |> knitr::kable(digits = 2) to the end of the accuracy() code above. 
# Nicely formatted table for the write-up (.qmd)
liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  select(.model, .type, RMSE, MAE) |>
  arrange(RMSE) |>
  knitr::kable(digits = 2,
               col.names = c("Model", "Type", "RMSE", "MAE"),
               caption = "Table 1: One-Month-Ahead Forecast Accuracy (TS-CV, Jan 2002 – Dec 2019)")

#Which model forecasts most accurately? Do the two metrics agree?



#2.4. Are the Differences Significant?

# Use a Diebold–Mariano test with squared error loss (power = 2) to test whether ARIMA and ETS are equally accurate:

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(
    as_tibble(liquor) |> select(Month, actual = sales),
    by = "Month"
  ) |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

# DM Test 1: ARIMA vs. ETS
dm_arima_ets <- forecast::dm.test(
  liquor_errs$arima,
  liquor_errs$ets,
  h     = 1,
  power = 2
)
dm_arima_ets

# DM Test 2: ARIMA vs. Seasonal Naive (modified)
dm_arima_snaive <- forecast::dm.test(
  liquor_errs$arima,
  liquor_errs$snaive,
  h     = 1,
  power = 2
)
dm_arima_snaive



#Part 3 – Forecast Combination

#3a. As we saw in class, a forecast combination in fable is just arithmetic on the fitted models. Create an equal-weight combination of ARIMA and ETS:
  
  liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

# #Report the results in a table: Equal-weight combination accuracy table. #Report the results in a table
liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  select(.model, RMSE, MAE) |>
  arrange(RMSE) |>
  knitr::kable(
    digits    = 2,
    col.names = c("Model", "RMSE", "MAE"),
    caption   = "Table 2: One-Month-Ahead Forecast Accuracy — Including Equal-Weight Combination (TS-CV)"
  )


#How does combo compare to each of the models that go into it?


#3b. We discussed in class that combining helps most when the forecast errors of the models are not too similar. Compute the correlation between the ARIMA and ETS forecast errors, using liquor_errs from Part 2.4:
  
  cor(liquor_errs$arima, liquor_errs$ets)

#Based on this correlation, and on the DM test in 2.4, explain in two or three sentences why the combination did or did not beat the best individual model.

  


