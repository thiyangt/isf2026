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

#############################################################
# install.packages("pak")
#pak::pak("thiyangt/vegcast")
## ---- packages
library(tidyverse)
library(imputeTS)
library(tsibble)
library(vegcast)
library(here)

## ---- loaddata
#veg_filled |> duplicates()
veg_filled |> head()
veg_filled |> tail()
veg_filled_imputed <- veg_filled |>
  mutate(Price_interp = round(na_interpolation(Price), 2)
  ) |>
  as_tsibble(key = c(Item, Type, Market),
             index = Date)

veg_filled_imputed |> print()
#veg_filled_imputed |> duplicates()
#veg_filled_imputed |> view()
veg_filled_imputed %>%
  count(Item, Type, Market, Date) %>%
  filter(n > 1)

## Splitdatainto training and test
train <- veg_filled_imputed |> filter(Year < 2026)
train |> head()
train |> tail()
test <- veg_filled_imputed |> filter(Year == 2026)
test |> head()
test |> tail()
test |> view()
##################################################
#ETS
library(fable)
library(dplyr)

fit_snaive <- train |>
  model(
    SNAIVE_model = SNAIVE(Price_interp)
  )
write.csv(fit_snaive,
          file=here("SNAIVE", "fit_snaive.csv"))

fc_snaive <- fit_snaive |>
  forecast(new_data = test)

write.csv(fc_snaive,
          file=here("SNAIVE", "fc_snaive.csv"))

accuracy_snaive <- fc_snaive |>
  accuracy(test)

accuracy_snaive
View(accuracy_snaive)
write.csv(accuracy_snaive,
          file=here("sNAIVE", "accuracy_snaive.csv"))

error_naive <- accuracy_naive |>
  group_by(Item, Type, Market) |>
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE)
  )

error_naive |>
  arrange(desc(RMSE)) |> print()
