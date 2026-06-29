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


