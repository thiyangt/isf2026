# run 
#1_preprocessing.R

library(readr)
library(tidyverse)
library(ranger)
library(Metrics)

# read the model
model_rf <- readRDS("model_rf.rds")
oob_pred <- model_rf$predictions


# Count missing values for each column
train <- read_csv(here("data", "train.csv"))
test <- read_csv(here("data", "test.csv"))
prf1 <- read_csv(here("data", "pred_rf1.csv"))
prf2 <- read_csv(here("data", "pred_rf2.csv"))

test$rf1 <- round(prf1$prediction, 2)
test$rf2 <- round(prf2$prediction, 2)


veg_test_eval <- test

# Compute error measures by group
error_rf1 <- veg_test_eval %>%
  group_by(Item, Type, Market) %>%
  mutate(Price_interp = round(Price_interp, 2)) %>%
  summarise(
    RMSE = rmse(Price_interp, rf1),
    MAE  = mae(Price_interp, rf1),
    MAPE = mape(Price_interp, rf1) * 100,
    .groups = "drop"
  )

# View results
error_rf1
View(error_rf1)


# Compute mean of errors across all groups
mean_errors_rf1 <- error_rf1 |>
  group_by(Item, Type, Market) %>%
  summarise(
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    mean_MAE  = mean(MAE, na.rm = TRUE),
    mean_MAPE = mean(MAPE, na.rm = TRUE)
  )

# View results
mean_errors_rf1
write_csv(mean_errors_rf1, here("results", "mean_errors_rf1.csv"))

###############################################
# rf2

# Compute error measures by group
error_rf2 <- veg_test_eval %>%
  group_by(Item, Type, Market) %>%
  mutate(Price_interp = round(Price_interp, 2)) %>%
  summarise(
    RMSE = rmse(Price_interp, rf2),
    MAE  = mae(Price_interp, rf2),
    MAPE = mape(Price_interp, rf2) * 100,
    .groups = "drop"
  )

# View results
error_rf2
View(error_rf2)


# Compute mean of errors across all groups
mean_errors_rf2 <- error_rf2 |>
  group_by(Item, Type, Market) %>%
  summarise(
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    mean_MAE  = mean(MAE, na.rm = TRUE),
    mean_MAPE = mean(MAPE, na.rm = TRUE)
  )

# View results
mean_errors_rf2
write_csv(mean_errors_rf2, here("results", "mean_errors_rf2.csv"))
