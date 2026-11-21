
install.packages("BVAR")
install.packages("urca")
install.packages("tidyverse")
install.packages("fpp3")

#library(tidyverse)
library(fpp3)
library(BVAR)
library(urca)

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



