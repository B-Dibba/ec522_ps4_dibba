
#install.packages("BVAR")
#install.packages("urca")
#install.packages("tidyverse")
#install.packages("fpp3")
#install.packages("purrr")

library(tidyverse)
library(fpp3)
library(BVAR)
library(urca)
library(readr) 
library(purrr) 

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

#Part 4 – A Medium-Scale VAR vs. a BVAR
  #4.1a. Download the file fredqd_2026-09.csv from Canvas and move it into the data/ folder inside your PS4 project. Then read it. Note that the first two rows after the header hold information about each series rather than data, so we remove them with slice().
  
  qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
    slice(-(1:2))


#4.1b.
  growth <- function(x) 100 * difference(log(x))
  
  qd <- qd_raw |>
    mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
    transmute(date,
              gdp    = growth(GDPC1),     # real GDP
              cons   = growth(PCECC96),   # real consumption
              inv    = growth(GPDIC1),    # real investment
              emp    = growth(PAYEMS),    # payroll employment
              infl   = growth(GDPCTPI),   # GDP price index
              unrate = difference(UNRATE),     # unemployment rate
              ffr    = difference(FEDFUNDS),   # federal funds rate
              gs10   = difference(GS10)) |>    # 10-year Treasury yield
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  
  y <- qd |> select(-date) |> as.matrix()
  
  nrow(y)
  head(y) 

  #4.1c. Set up the TS-CV. Our first training sample is the first 120 quarters, 1960 Q1–1989 Q4, and we then forecast one quarter ahead with an expanding window:
  
  N <- nrow(y)
  origins <- 120:(N - 1)
  actual <- y[121:N, ]
  
  #How many forecasts will we produce for each variable, and what are the dates of the first and last forecast?
  
  #4.2. OLS VARs
  #Recall from class that an OLS VAR is just a regression for each variable on the same set of lags. The function below estimates a VAR(\(p\)) on the data in z with lm(), then uses the estimated coefficients to forecast the next period. We then use a loop to re-estimate the model at each forecast origin T and stack the forecasts into a matrix.
  var_ols_fc <- function(z, p) {
    e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
    M <- ncol(z)                    # number of variables
    B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
    x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
    x %*% B
  }
  
  fc_var1 <- NULL
  fc_var4 <- NULL
  for (T in origins) {
    fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
    fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
  }
  
  #Compute the RMSE of each model for each variable:
    
    rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))
  
  rmse_by_var(fc_var1)
  rmse_by_var(fc_var4)
  
  #Which VAR forecasts better? For how many of the eight variables? Given that the VAR(4) contains the VAR(1) as a special case, why might the smaller model forecast better?
  
  
#4.3. A BVAR
  #4.3a. Now estimate a BVAR(4) with a Minnesota prior. Since all of our series are growth rates or changes, we use a prior mean of b = 0 on each series’ own first lag. We put the code in a function so that you can easily try a different value of \(\lambda\). The min and max arguments hold \(\lambda\) at (essentially) the value we choose, rather than letting the data estimate it. To keep the run time down, we use fewer simulation draws (n_draw, n_burn) than the default. This will take about 1 to 2 minutes for each value of \(\lambda\).
  
  bvar_cv <- function(lambda) {
    pr <- bv_priors(hyper = "lambda",
                    mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                  min = 0.999 * lambda,
                                                  max = 1.001 * lambda),
                               b = 0))
    fc <- NULL
    for (T in origins) {
      fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                    n_draw = 2000, n_burn = 1000, verbose = FALSE)
      x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
      fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
    }
    fc
  }
  
  set.seed(422)
  fc_bvar_03 <- bvar_cv(0.3)
  #saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")
  #readRDS(fc_bvar_03, "results/fc_bvar_03.rds") #for write up
  
  
#4.3b. Recall that a small \(\lambda\) means heavy shrinkage. Repeat 4.3a with a much tighter prior, \(\lambda = 0.05\). Then collect all of your results in a table and save it:
    
    set.seed(422)
  fc_bvar_005 <- bvar_cv(0.05)
  
  var_results <- tibble(
    Variable = colnames(y),
    `VAR(1)` = rmse_by_var(fc_var1),
    `VAR(4)` = rmse_by_var(fc_var4),
    `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
    `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
  )
  
  #saveRDS(var_results, "results/var_results.rds")
  #readRDS(var_results, "results/var_results.rds") #For write up
  
  fc_bvar_03  <- readRDS("results/fc_bvar_03.rds")
  var_results <- readRDS("results/var_results.rds")
  
  var_results |> knitr::kable(digits = 3)
  
  #In your write-up, your .qmd can load this table with readRDS() rather than re-running the horse race. This will save you a significant amount of computation time. Answer the following:
    
    #How does the BVAR(4) with \(\lambda = 0.3\) compare to the OLS VAR(4)? Both have the same variables and the same lags, so what explains any difference?
    
    #How does it compare to the VAR(1)?
    
    #What happens when \(\lambda = 0.05\)? What does this tell you about the amount of shrinkage? Suppose you used this table to choose your forecasting model. Would the winner’s RMSE here be a fair estimate of how accurately it will forecast in the future? Why or why not?

  #4.3c. For GDP growth, test whether the VAR(4) and the BVAR(4) with \(\lambda = 0.3\) are equally accurate, using a Diebold–Mariano test with squared error loss. Then write the code to repeat the test for a second variable of your choice (i.e. not GDP growth). What do you conclude? How does this affect how you think about your results in 4.3b?
  
  e_var4 <- actual[, "gdp"] - fc_var4[, 1]
  e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]
  
  forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)
  
  # Second variable: consumption (column 2)
  e_var4_cons <- actual[, "cons"] - fc_var4[, 2]
  e_bvar_cons <- actual[, "cons"] - fc_bvar_03[, 2]
  
  forecast::dm.test(e_var4_cons, e_bvar_cons, h = 1, power = 2)
  

#Part 5 – A Factor Model with FRED-MD
  #5.1a. Download the September 2026 vintage of FRED-MD, the file fredmd_2026-09.csv, from Canvas and move it into the data/ folder inside your PS4 project. The first row after the header gives a transformation code for each series, which says how McCracken and Ng recommend transforming it before use. For example, code 5 means the growth rate (100 times the first difference of the log). The function below applies these codes.
  
  md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
  tcode <- unlist(md_raw[1, -1])
  md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))
  
  transform_series <- function(x, code) {
    d <- function(v) c(NA, diff(v))
    switch(as.character(code),
           "1" = x,                      # level
           "2" = d(x),                   # first difference
           "3" = d(d(x)),                # second difference
           "4" = log(x),                 # log
           "5" = 100 * d(log(x)),        # growth rate
           "6" = 100 * d(d(log(x))),     # change in the growth rate
           "7" = d(x / dplyr::lag(x) - 1))
  }
  
  
  #5.1b. Apply the transformations, keep the same sample as in class (March 1960–December 2019), and drop any series with missing values in that sample. You should end up with 718 rows and 122 columns (the date and 121 series).

  fm_dates <- as.Date(md[["sasdate"]], "%m/%d/%Y")
  
  fm <- bind_cols(map2(md[names(tcode)], tcode, transform_series)) |>
    mutate(date = fm_dates, .before = 1) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  
  fm <- fm[, colSums(is.na(fm)) == 0]
  dim(fm)   # should be 718 × 122

  #5.1c. Run tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]. 
  tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]
  
  #In a sentence for each: how is each of these four series transformed? Why do you think the transformation chosen for FEDFUNDS differs from the one for INDPRO?

#5.2a. Estimate the factors by principal components, and find the share of the variation they explain:  
  Z <- fm |> select(-date) |> as.matrix()
  
  pc <- prcomp(scale(Z))
  
  summary(pc)$importance[, 1:5]
  
  
#5.2b. The sign of a factor is arbitrary, so we flip the first factor if needed to make it rise with industrial production. Plot it:
  
  F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))
  
  tibble(date = fm$date, F1 = F1) |>
    ggplot(aes(date, F1)) +
    geom_line() +
    labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
    theme_minimal()
  
#5.2c. Find the eight series most correlated with each of the first two factors:
    
  sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
  sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]
  
#The names of the series can be looked up in the FRED-MD documentation, FRED-MD_updated_appendix.pdf, which is posted on Canvas. Based on these series, and on your plot, how would you describe what each factor captures?  

#5.3. Replicate the Horse Race from Class  
  
  ip <- fm$INDPRO
  n <- nrow(Z)
  init <- which(fm$date == as.Date("1989-12-01"))
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - 1)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]         # factors using data through T
    d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
    new <- data.frame(y = ip[T], t(F_T[T, ]))
    e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
    e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
  }
  
  c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))
  
#In class we saw an RMSE of 0.617 for the AR(1) and 0.589 with the factors. Confirm that you get the same numbers. Why would it be cheating to estimate the factors once, using the full sample, and use those in the horse race?
  
  
  #5.4 A Direct Forecast of Inflation
  
  direct_race <- function(series, h = 12, r = 3) {
    lv <- md |>
      mutate(log_x = log(.data[[series]]),
             g = 100 * (log_x - lag(log_x))) |>
      filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
    yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
    g  <- lv$g                                      # growth over the latest month
    
    e_ar <- NULL
    e_fa <- NULL
    for (T in init:(n - h)) {
      F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
      i <- 1:(T - h)
      d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
      new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
      e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
      e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
    }
    tibble(series, forecasts = length(e_ar),
           RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
           MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
           DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                         varestimator = "bartlett")$p.value)
  }
  
  direct_race("CPIAUCSL")
  
  
  
  
  
  
  