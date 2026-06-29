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

train <- veg_filled_imputed |>
  filter(Date < "2026-01-01")
################################################
fit_snaive <- train |>
  model(
    SNAIVE_model = SNAIVE(Price_interp)
  )

test7 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-08")
fc_snaive7 <- fit_snaive |>
  forecast(new_data = test7)
accuracy_snaive7 <- fc_snaive7 |>
  accuracy(test7)
write_csv(accuracy_snaive7, "rollinforecaserrors/accuracy_snaive7.csv")

test14 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-15")
fc_snaive14 <- fit_snaive |>
  forecast(new_data = test14)
accuracy_snaive14 <- fc_snaive14 |>
  accuracy(test14)
write_csv(accuracy_snaive14, "rollinforecaserrors/accuracy_snaive14.csv")

test21 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-22")
fc_snaive21 <- fit_snaive |>
  forecast(new_data = test21)
accuracy_snaive21 <- fc_snaive21 |>
  accuracy(test21)
write_csv(accuracy_snaive21, "rollinforecaserrors/accuracy_snaive21.csv")

test28 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-29")
fc_snaive28 <- fit_snaive |>
  forecast(new_data = test28)
accuracy_snaive28 <- fc_snaive28 |>
  accuracy(test28)
write_csv(accuracy_snaive28, "rollinforecaserrors/accuracy_snaive28.csv")

################################################
fit_naive <- train |>
  model(
    NAIVE_model = NAIVE(Price_interp)
  )

test7 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-08")
fc_naive7 <- fit_naive |>
  forecast(new_data = test7)
accuracy_naive7 <- fc_naive7 |>
  accuracy(test7)
write_csv(accuracy_naive7, "rollinforecaserrors/accuracy_naive7.csv")

test14 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-15")
fc_naive14 <- fit_naive |>
  forecast(new_data = test14)
accuracy_naive14 <- fc_naive14 |>
  accuracy(test14)
write_csv(accuracy_naive14, "rollinforecaserrors/accuracy_naive14.csv")

test21 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-22")
fc_naive21 <- fit_naive |>
  forecast(new_data = test21)
accuracy_naive21 <- fc_naive21 |>
  accuracy(test21)
write_csv(accuracy_naive21, "rollinforecaserrors/accuracy_naive21.csv")

test28 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-29")
fc_naive28 <- fit_naive |>
  forecast(new_data = test28)
accuracy_naive28 <- fc_naive28 |>
  accuracy(test28)
write_csv(accuracy_naive28, "rollinforecaserrors/accuracy_naive28.csv")

################################################
# ARIMA
fit_arima <- train |>
  model(
    ARIMA_model = ARIMA(Price_interp)
  )

test7 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-08")
fc_arima7 <- fit_arima |>
  forecast(new_data = test7)
accuracy_arima7 <- fc_arima7 |>
  accuracy(test7)
write_csv(accuracy_arima7, "rollinforecaserrors/accuracy_arima7.csv")

test14 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-15")
fc_arima14 <- fit_arima |>
  forecast(new_data = test14)
accuracy_arima14 <- fc_arima14 |>
  accuracy(test14)
write_csv(accuracy_arima14, "rollinforecaserrors/accuracy_arima14.csv")

test21 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-22")
fc_arima21 <- fit_arima |>
  forecast(new_data = test21)
accuracy_arima21 <- fc_arima21 |>
  accuracy(test21)
write_csv(accuracy_arima21, "rollinforecaserrors/accuracy_arima21.csv")

test28 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-29")
fc_arima28 <- fit_arima |>
  forecast(new_data = test28)
accuracy_arima28 <- fc_arima28 |>
  accuracy(test28)
write_csv(accuracy_arima28, "rollinforecaserrors/accuracy_arima28.csv")

################################################
# STL_ARIMA
fit_stlarima <- train |>
  model(
    STL_ARIMA = decomposition_model(
      STL(Price_interp),
      ARIMA(season_adjust)
    )
  )

test7 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-08")
fc_stlarima7 <- fit_stlarima |>
  forecast(new_data = test7)
accuracy_stlarima7 <- fc_stlarima7 |>
  accuracy(test7)
write_csv(accuracy_stlarima7, "rollinforecaserrors/accuracy_stlarima7.csv")

test14 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-15")
fc_stlarima14 <- fit_stlarima |>
  forecast(new_data = test14)
accuracy_stlarima14 <- fc_stlarima14 |>
  accuracy(test14)
write_csv(accuracy_stlarima14, "rollinforecaserrors/accuracy_stlarima14.csv")

test21 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-22")
fc_stlarima21 <- fit_stlarima |>
  forecast(new_data = test21)
accuracy_stlarima21 <- fc_stlarima21 |>
  accuracy(test21)
write_csv(accuracy_stlarima21, "rollinforecaserrors/accuracy_stlarima21.csv")

test28 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-29")
fc_stlarima28 <- fit_stlarima |>
  forecast(new_data = test28)
accuracy_stlarima28 <- fc_stlarima28 |>
  accuracy(test28)
write_csv(accuracy_stlarima28, "rollinforecaserrors/accuracy_stlarima28.csv")

################################################
# STL_ETS
fit_stlets <- train |>
  model(
    STL_ETS = decomposition_model(
      STL(Price_interp),
      ETS(season_adjust)
    )
  )

test7 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-08")
fc_stlets7 <- fit_stlets |>
  forecast(new_data = test7)
accuracy_stlets7 <- fc_stlets7 |>
  accuracy(test7)
write_csv(accuracy_stlets7, "rollinforecaserrors/accuracy_stlets7.csv")

test14 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-15")
fc_stlets14 <- fit_stlets |>
  forecast(new_data = test14)
accuracy_stlets14 <- fc_stlets14 |>
  accuracy(test14)
write_csv(accuracy_stlets14, "rollinforecaserrors/accuracy_stlets14.csv")

test21 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-22")
fc_stlets21 <- fit_stlets |>
  forecast(new_data = test21)
accuracy_stlets21 <- fc_stlets21 |>
  accuracy(test21)
write_csv(accuracy_stlets21, "rollinforecaserrors/accuracy_stlets21.csv")

test28 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-29")
fc_stlets28 <- fit_stlets |>
  forecast(new_data = test28)
accuracy_stlets28 <- fc_stlets28 |>
  accuracy(test28)
write_csv(accuracy_stlets28, "rollinforecaserrors/accuracy_stlets28.csv")

## rf
rollingforecast <- read_csv("rollingforecast.csv")
head(rollingforecast)
rollingforecast2026 <- rollingforecast |>
  filter(Date > "2025-12-31") |>
  rename(prediction = Price_interp)
View(rollingforecast2026)
test28 <- veg_filled_imputed |> filter(Date > "2025-12-31") |>
  filter(Date < "2026-01-29")

rollingfull <- rollingforecast2026 |>
  left_join(
    test28 |>
      select(Date, Item, Type, Market, Price_interp),
    by = c("Date", "Item", "Type", "Market")
  )
View(rollingfull)  

library(dplyr)
rollingfull7 <- rollingfull |>
  filter(Date < "2026-01-08")

accuracy_rf7 <- rollingfull7 |>
  group_by(Item, Type, Market) |>
  summarise(
    RMSE = sqrt(mean((prediction - Price_interp)^2, na.rm = TRUE)),
    
    MAE = mean(abs(prediction - Price_interp), na.rm = TRUE),
    
    MPE = mean((prediction - Price_interp) / Price_interp * 100, na.rm = TRUE),
    
    MAPE = mean(abs((prediction - Price_interp) / Price_interp) * 100, na.rm = TRUE),
    
    MASE = NA_real_   # needs in-sample MAE for scaling
  )

write_csv(accuracy_rf7, "rollinforecaserrors/accuracy_rf7.csv")

rollingfull14 <- rollingfull |>
  filter(Date < "2026-01-15")

accuracy_rf14 <- rollingfull14 |>
  group_by(Item, Type, Market) |>
  summarise(
    RMSE = sqrt(mean((prediction - Price_interp)^2, na.rm = TRUE)),
    
    MAE = mean(abs(prediction - Price_interp), na.rm = TRUE),
    
    MPE = mean((prediction - Price_interp) / Price_interp * 100, na.rm = TRUE),
    
    MAPE = mean(abs((prediction - Price_interp) / Price_interp) * 100, na.rm = TRUE),
    
    MASE = NA_real_   # needs in-sample MAE for scaling
  )

write_csv(accuracy_rf14, "rollinforecaserrors/accuracy_rf14.csv")



rollingfull21 <- rollingfull |>
  filter(Date < "2026-01-22")

accuracy_rf21 <- rollingfull21 |>
  group_by(Item, Type, Market) |>
  summarise(
    RMSE = sqrt(mean((prediction - Price_interp)^2, na.rm = TRUE)),
    
    MAE = mean(abs(prediction - Price_interp), na.rm = TRUE),
    
    MPE = mean((prediction - Price_interp) / Price_interp * 100, na.rm = TRUE),
    
    MAPE = mean(abs((prediction - Price_interp) / Price_interp) * 100, na.rm = TRUE),
    
    MASE = NA_real_   # needs in-sample MAE for scaling
  )

write_csv(accuracy_rf21, "rollinforecaserrors/accuracy_rf21.csv")

rollingfull28 <- rollingfull |>
  filter(Date < "2026-01-29")

accuracy_rf28 <- rollingfull28 |>
  group_by(Item, Type, Market) |>
  summarise(
    RMSE = sqrt(mean((prediction - Price_interp)^2, na.rm = TRUE)),
    
    MAE = mean(abs(prediction - Price_interp), na.rm = TRUE),
    
    MPE = mean((prediction - Price_interp) / Price_interp * 100, na.rm = TRUE),
    
    MAPE = mean(abs((prediction - Price_interp) / Price_interp) * 100, na.rm = TRUE),
    
    MASE = NA_real_   # needs in-sample MAE for scaling
  )

write_csv(accuracy_rf28, "rollinforecaserrors/accuracy_rf28.csv")
