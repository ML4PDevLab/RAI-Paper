library(tradestatistics)
library(readr)
library(tidyverse)
library(dplyr)
library(estimatr)
library(car)
library(tibble)
library(usethis)
library(tidyr)
library(progress)
library(furrr)
library(magrittr)

# Set working directory and load data
setwd('/Users/johnazubuike/Downloads/dataverse_files')

TradeDataSwitched <- ots_create_tidy_data(
  years = 2016:2020,
  reporters = c("Russia"),
  partners = c("all"),
  commodities = c("all"),
  table = "yrpc"
)
average_trade_data <- TradeDataSwitched %>%
  group_by(partner_name, commodity_code) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

view(average_trade_data)
view(TradeDataSwitched)

#load DFPR dataset
## ADD CODE HERE

#correct names to match DFPR
TradeDataSwitched <- TradeDataSwitched %>%
  mutate(partner_name = case_when(
    partner_name == "Rep. of Moldova" ~ "Moldova",
    partner_name == "United Rep. of Tanzania" ~ "Tanzania",
    partner_name == "TFYR of Macedonia" ~ "North Macedonia",
    partner_name == "Dem. Rep. of the Congo" ~ "Congo, Democratic Republic of",
    TRUE ~ partner_name
  ))

# Calculate the trade balance, HS2, and HS4
TradeDataSwitched <- TradeDataSwitched %>%
  mutate(
    trade_value_usd_tbal = trade_value_usd_exp - trade_value_usd_imp,
    HS2 = substr(commodity_code, 1, 2),
    HS4 = substr(commodity_code, 1, 4)
  )

###

# Aggregate exports, imports, and trade balance by year and HS2
aggregated_data_hs2 <- TradeDataSwitched %>%
  group_by(year, HS2) %>%
  summarise(
    total_exports = sum(trade_value_usd_exp, na.rm = TRUE),
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    total_trade_balance = sum(trade_value_usd_tbal, na.rm = TRUE)
  ) %>%
  mutate(
    export_rank = dense_rank(desc(total_exports)),
    import_rank = dense_rank(desc(total_imports)),
    trade_balance_rank = dense_rank(total_trade_balance),
    import_factor = total_imports / (total_imports + total_exports),
    export_factor = total_exports / (total_imports + total_exports),    
    trade_share = (total_imports + total_exports) / sum(total_imports + total_exports, na.rm = TRUE),
    major_import = ifelse(import_factor > 0.8 & trade_share > 0.01, 1, 0),
    major_export = ifelse(export_factor > 0.8 & trade_share > 0.01, 1, 0)
  ) %>%
  ungroup()
view(aggregated_data_hs2)
view(filter(aggregated_data_hs2, major_import==1))
view(filter(aggregated_data_hs2, major_export==1))

# Aggregate exports, imports, and trade balance by year and HS4
aggregated_data_hs4 <- TradeDataSwitched %>%
  group_by(year, HS4) %>%
  summarise(
    total_exports = sum(trade_value_usd_exp, na.rm = TRUE),
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    total_trade_balance = sum(trade_value_usd_tbal, na.rm = TRUE)
  ) %>%
  mutate(
    export_rank = dense_rank(desc(total_exports)),
    import_rank = dense_rank(desc(total_imports)),
    trade_balance_rank = dense_rank(total_trade_balance),
    import_factor = total_imports / (total_imports + total_exports),
    trade_share = (total_imports + total_exports) / sum(total_imports + total_exports, na.rm = TRUE)
  ) %>%
  ungroup()
view(filter(DFPR_TradeData, HS2==30))
# Aggregate exports, imports, and trade balance by year and commodity_code
aggregated_data_commodity <- TradeDataSwitched %>%
  group_by(year, commodity_code) %>%
  summarise(
    total_exports = sum(trade_value_usd_exp, na.rm = TRUE),
    total_imports = sum(trade_value_usd_imp, na.rm = TRUE),
    total_trade_balance = sum(trade_value_usd_tbal, na.rm = TRUE)
  ) %>%
  mutate(
    export_rank = dense_rank(desc(total_exports)),
    import_rank = dense_rank(desc(total_imports)),
    trade_balance_rank = dense_rank(total_trade_balance),
    import_factor = total_imports / (total_imports + total_exports),
    trade_share = (total_imports + total_exports) / sum(total_imports + total_exports, na.rm = TRUE)
  ) %>%
  ungroup()

###

# Add DFPR demarkator, (descending) rankings for import, export and trade balance
# Add import_factor, export_factor, and tradeshare
PreDFPR_TradeData <- TradeDataSwitched %>%
  mutate(
    DFPR = ifelse(partner_name %in% dfpr$country, 1, 0),
    export_rank = dense_rank(desc(trade_value_usd_exp)),
    import_rank = dense_rank(desc(trade_value_usd_imp)),
    trade_balance_rank = dense_rank(trade_value_usd_tbal),
    import_factor = trade_value_usd_imp / (trade_value_usd_imp + trade_value_usd_exp),
    export_factor = trade_value_usd_exp / (trade_value_usd_imp + trade_value_usd_exp),
    trade_share = (trade_value_usd_imp + trade_value_usd_exp) / sum(trade_value_usd_imp + trade_value_usd_exp, na.rm = TRUE)
  )%>%
  left_join(aggregated_data_hs2 %>% select(HS2, major_import, major_export), by = "HS2")

# Step 2: Add major_imports and major_exports columns
#PreDFPR_TradeData <- PreDFPR_TradeData %>%
#  mutate(major_imports = ifelse(HS2 %in% c("84", "85", "87", "30", "90"), 1, 0),
#         major_exports = ifelse(HS2 %in% c("27", "72", "99", "31", "76", "44", "10", "71"), 1, 0))

# Step 3: Merge columns from dfpr to DFPR_TradeData
## Add Code Here

# Print the first few rows of the new dataset to verify the changes
view(PreDFPR_TradeData)

# Perform the regression analysis
## Add Code Here

# Summarize the regression model
## Add Code Here
