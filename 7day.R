#packages
library(tidyverse)


#data
fc_arima <- read_csv("ARIMA/fc_arima.csv")
fc_ets <- read_csv("ETS/fc_ets.csv")
fc_naive <- read_csv("NAIVE/fc_naive.csv")
fc_snaive <- read_csv("SNAIVE/fc_snaive.csv")
fc_stlarima <- read_csv("STL_ARIMA/fc_stlarima.csv")
fc_stlets <- read_csv("STL_ETS/fc_stlets.csv")
fc_rfdouble <- read_csv("rollingforecast.csv")
View(fc_arima)
View(fc_ets)
View(fc_naive)
View(fc_snaive)
View(fc_stlarima)
View(fc_stlets)
View(fc_rfdouble)

fc_rfdouble <- fc_rfdouble |>
  filter(Year > 2025)
View(fc_rfdouble)
fc_arima7 <- fc_arima |> filter(Date < "2026-01-08")
fc_ets7 <- fc_ets |> filter(Date < "2026-01-08")
fc_naive7 <- fc_naive |> filter(Date < "2026-01-08")
fc_stlarima7 <- fc_stlarima |> filter(Date < "2026-01-08")
fc_stlets7 <- fc_stlets |> filter(Date < "2026-01-08")
fc_rfdouble7 <- fc_rfdouble |> filter(Date < "2026-01-08")
fc_snaive7 <- fc_snaive |> filter(Date < "2026-01-08")

dim(fc_arima7) 
dim(fc_ets7)
dim(fc_naive7)
dim(fc_stlarima7)
dim(fc_stlets7)
dim(fc_rfdouble7)

library(fable)
library(dplyr)

####################################################
library(tidyverse)
#install.packages("pak")
#pak::pak("thiyangt/vegetablesSriLanka")
library("vegetablesSriLanka")
library(knitr)
library(plotly)
library(GGally)
library(patchwork)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(viridis)
library(devtools)
#install_github("SteffenMoritz/imputeTS")
library(imputeTS)


vegetables.srilanka <- vegetables.srilanka |>
  mutate( Item = factor(Item, levels = c("Beans",
                                         "Big Onion (Imp)",
                                         "Brinjal",
                                         "Cabbage",
                                         "Carrot",
                                         "Green Chilli",
                                         "Lime",
                                         "Potato (Imp)",
                                         "Pumpkin",
                                         "Snake gourd",
                                         "Tomato",
                                         "Potato (Local)",
                                         "Big Onion (Local)", "Red Onion (Imp)",
                                         "Red Onion (Local)"))) 

vegetables_long_tsibble <- vegetables.srilanka |>
  as_tsibble(key = c(Item, Type, Market),
             index = Date) 

veg_filled <- vegetables_long_tsibble |>
  group_by_key() |>
  fill_gaps() |>
  ungroup() |>
  as_tsibble() |>
  mutate(Year = year(Date))

veg_filled_imputed <- veg_filled |>
  mutate(Price_interp = round(na_interpolation(Price), 2)
  ) |>
  as_tsibble(key = c(Item, Type, Market),
             index = Date)


################################################
test7 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-08")
accuracy_snaive7 <- fc_snaive7 |>
  accuracy(test7)

library(dplyr)

eval_data_snaive7 <- test7 %>%
  select(Date, Item, Type, Market, Price_interp) %>%
  left_join(
    fc_snaive7 %>%
      select(Date, Item, Type, Market, .mean),
    by = c("Date", "Item", "Type", "Market")
  )

rmse_snaive7 <- sqrt(mean((eval_data_snaive7$Price_interp - eval_data_snaive7$.mean)^2, na.rm = TRUE))
rmse
