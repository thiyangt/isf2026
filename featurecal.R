# run 
#1_preprocessing.R

library(readr)
library(tidyverse)


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

library(feasts)
library(tsibble)
library(feast)
library(dplyr)

# Compute features grouped by Item, Type, Market
features_train <- train %>%
  group_by(Item, Type, Market) %>%
  features(Price_interp, feature_set(pkgs = "feasts"))

# Inspect
glimpse(features_train)
dim(features_train)

library(dplyr)
library(ggplot2)

# Select only numeric feature columns for PCA
feature_cols <- features_train %>% 
  select(where(is.numeric))

# Optionally, handle any Inf or NA values
library(dplyr)

# Keep only numeric columns that are not constant
feature_cols <- features_train %>%
  select(where(is.numeric)) %>%
  select(where(~n_distinct(.) > 1))   # remove constant columns

pca_result <- prcomp(feature_cols, scale. = TRUE)  # scale features

features_train_pca <- features_train %>%
  slice(1:nrow(pca_result$x)) %>%
  bind_cols(as.data.frame(pca_result$x))
write.csv(features_train_pca, file = here("results", "features_train_pca.csv"))
library(ggplot2)

ggplot(features_train_pca, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA: PC2 vs PC1", x = "PC1", y = "PC2")

ggplot(features_train_pca, aes(x = PC1, y = PC2, color = Market)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA: PC2 vs PC1", x = "PC1", y = "PC2")

ggplot(features_train_pca, aes(x = PC1, y = PC2, color = Item)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA: PC2 vs PC1", x = "PC1", y = "PC2")
