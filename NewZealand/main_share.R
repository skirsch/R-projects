## data obtained from
## https://mpidr.shinyapps.io/stmortality/


if(!require(pacman)) install.packages("pacman")
# pacman::p_unlock() ## removes any locks from failed installs.

#update.packages(ask = FALSE)

pacman::p_load(magrittr, tidyverse, rio, stringi, GGally,
               stringr, ggplot2, ggseas, forecast)

## load data
df <- rio::import("./NewZealand/data/Mortality_NZ_2023.xlsx", sheet = "NZL_NP")

# duplicate check----
stopifnot(df[duplicated(df),] %>% nrow == 0)



df_ts <- df |>  subset(`Sex` == "b")
df_ts <- df_ts[, c("Year", "Week", "Death_rates_Total")]
df_ts$Year_week <- df_ts$`Year` + df_ts$`Week`/52
df_ts <- df_ts[ order(df_ts$Year_week, decreasing = FALSE), ]
names(df_ts) <- str_replace_all(names(df_ts), " ", "_")
df_ts$Rdate <- as.Date(paste(df_ts$Year, df_ts$Week, "1", sep = "-"), "%Y-%U-%u")
df_ts$Rdate <- as.Date(paste(df_ts$Year, df_ts$Week, 1, sep="-"), "%Y-%U-%u")
df_ts$Rdate

time_series <- ts(data = df_ts[, "Death_rates_Total"], frequency = 52, start = 2010+51/52)
time_series

## Basic

up <- stl(time_series, "periodic")

plot(up, main = "Death rates NZ")

## Fancy

autoplot(up)
time_series  |> stats::decompose()  |>  ggplot2::autoplot() +
  ggtitle("Weekly death counts all causes NZ: \n2010 to November 2023")



