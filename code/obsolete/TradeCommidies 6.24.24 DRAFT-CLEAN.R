library(tradestatistics)
library(readr)
library(tidyverse)
library(dplyr)
library(estimatr)
library(car)
library(tibble)
library(usethis)
library(tidyr)
library(here)

setwd(here())

# Generate a data frame containing trade data for the years 2016-2020 where Russia as a trade partner across all countries and commodities

TradeData <- ots_create_tidy_data(
  years = 2016:2020,
  reporters = c("all"),
  partners = c("Russia"),
  commodities = c("all"),
  table = "yrpc"
)
view(TradeData)
write.csv(TradeData, here(), row.names = FALSE)

# Add Trade Balance, Harmonic System 2 category, Harmonic System 4 category, and import factor ratio columns

TradeData <- TradeData %>%
  mutate(
    trade_value_usd_tbal = trade_value_usd_exp - trade_value_usd_imp,
    HS2 = substr(commodity_code, 1, 2),
    HS4 = substr(commodity_code, 1, 4),
    ImportFactor = trade_value_usd_imp/trade_value_usd_tbal
  )
view(TradeData)

# Custom ranking function that assigns 0 to zeros

custom_rank <- function(x) {
  ranks <- rank(-x, ties.method = "first")
  ranks[x == 0] <- 0
  return(ranks)
}

# Count the number of rows with a higher trade_value_usd_tbal

TradeData <- TradeData %>%
  arrange(year, reporter_name, trade_value_usd_tbal, HS2) %>%
  group_by(year, commodity_code, HS2) %>%
  mutate(
    my_ranks = custom_rank(trade_value_usd_imp),
    count_higher_tbal = -(sum(trade_value_usd_tbal > trade_value_usd_tbal) - row_number() + 1),
    count_lower_tbal = row_number() + (sum(trade_value_usd_tbal < trade_value_usd_tbal))
  ) %>%
  ungroup()

# Create wide data set with countries along the rows and commodity codes along the columns 
## WARNING may hang up RStudio

TradeDataWide <- TradeData %>%
  select(reporter_name, commodity_code, my_ranks) %>%
  spread(key = commodity_code, value = my_ranks)

# Merge the Separate Dataframes 

TradeDataWideRows = bind_rows(TradeDataWide2, TradeDataWide3, TradeDataWide4, TradeDataWide5, TradeDataWide6, TradeDataWide7, TradeDataWide8, TradeDataWide9)
view(TradeDataWideRows)

# [HOLD] Code to count the number of top X ranks


  
# Break Trade Data into chunks of 100k rows and tranform it from long data to wide data with countries along the rows and commodity codes along the columns 
  
  subset_1=TradeData[1:100000,]
view(subset_1)

TradeDataWide2 <- subset_1 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide2)

subset_2=TradeData[100001:200000,]
view(subset_2)

TradeDataWide3 <- subset_2 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide3)

subset_3=TradeData[200001:300000,]
view(subset_3)

TradeDataWide4 <- subset_3 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide4)

subset_4=TradeData[300001:400000,]
view(subset_4)

TradeDataWide5 <- subset_4 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide5)

subset_5=TradeData[400001:500000,]
view(subset_5)

TradeDataWide6 <- subset_5 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide6)

subset_6=TradeData[500001:600000,]
view(subset_6)

TradeDataWide7 <- subset_6 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide7)

subset_7=TradeData[600001:700000,]
view(subset_7)

TradeDataWide8 <- subset_7 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide8)

subset_8=TradeData[700001:707969,]
view(subset_8)

TradeDataWide9 <- subset_8 %>%
  select(reporter_name, commodity_code, count_higher_tbal) %>%
  spread(key = commodity_code, value = count_higher_tbal)
view(TradeDataWide9)

