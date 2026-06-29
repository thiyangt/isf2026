library(tidyverse)
arima7 <- read_csv("rollinforecaserrors/accuracy_arima7.csv")
naive7 <- read_csv("rollinforecaserrors/accuracy_naive7.csv")
snaive7 <- read_csv("rollinforecaserrors/accuracy_snaive7.csv")
rf7 <- read_csv("rollinforecaserrors/accuracy_rf7.csv")
stlets7 <- read_csv("rollinforecaserrors/accuracy_stlets7.csv")
stlarima7 <- read_csv("rollinforecaserrors/accuracy_stlarima7.csv")

rf7 <- rf7 |>
  left_join(
    naive7 |>
      select(Item, Type, Market, MAPE) |>
      rename(NAIVE = MAPE),
    by = c("Item", "Type", "Market")
  ) |> left_join(
    arima7 |>
      select(Item, Type, Market, MAPE) |>
      rename(ARIMA = MAPE),
    by = c("Item", "Type", "Market")
  ) |> left_join(
    snaive7 |>
      select(Item, Type, Market, MAPE) |>
      rename(SNAIVE = MAPE),
    by = c("Item", "Type", "Market")
  ) |> left_join(
    stlets7 |>
      select(Item, Type, Market, MAPE) |>
      rename(STLETS = MAPE),
    by = c("Item", "Type", "Market")
  ) |> left_join(
    stlarima7 |>
      select(Item, Type, Market, MAPE) |>
      rename(STLARIMA = MAPE),
    by = c("Item", "Type", "Market")
  ) |>
  rename(RF = MAPE)
View(rf7)

library(dplyr)
library(tidyr)

rf_long7 <- rf7 |>
  pivot_longer(
    cols = c(RF, NAIVE, SNAIVE, ARIMA, STLETS, STLARIMA),
    names_to = "Model",
    values_to = "MAPE"
  )

library(dplyr)
library(ggplot2)

rf_long7 |>
  filter(Type == "Retail", Market == "Dambulla") |>
  mutate(
    MAPE = round(MAPE, 2),
    Model = factor(Model,
                   levels = c("NAIVE", "SNAIVE", "ARIMA", "STLETS", "STLARIMA", "RF"))
  ) |>
  ggplot(aes(x = Model, y = MAPE)) +
  geom_col() +
  facet_wrap(~Item) +
  geom_text(aes(label = MAPE), vjust = -0.3) +
  labs(x = "Model", y = "MAPE (%)") +
  theme_minimal()
