# This code code pulls UN Ideal Point Data from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ
# The files are too large to store in git, so we must store the source files locally
# and then extract the information we need before storing it in the repo.
# Note: COD does not have 2021 data for some reason
# Note: XKX does not have UN Ideal Point data

library(readr)
library(dplyr)
library(tidyr)

# Set local pathway depending on user
path_root <- function(...) {
  if (Sys.info()["user"]=="jeremy") {
    base <- "/home/jeremy/Downloads"
  }
  if (Sys.info()["user"]=="johnazubuike") {
    base <- "/Users/johnazubuike/Downloads"
  }  
  file.path(base, ...)
}

# Define the path to the data file
data_file <- path_root("dataverse_files/AgreementScoresAll_Sep2023.csv")

# Load the dataset
data <- read_csv(data_file)

# Subset relevant data and average years
filtered_data <- data %>%
  filter(year > 2016 & year < 2021 & ccode2 %in% c(365, 2, 220, 200, 710)) %>%
  mutate(country = case_when(
    ccode2 == 365 ~ "RUS",
    ccode2 == 2 ~ "USA",
    ccode2 == 220 ~ "FRA",
    ccode2 == 200 ~ "UKO",
    ccode2 == 710 ~ "CHN"
  )) %>%
  select(ccode1, country, year, IdealPointDistance)

# Calculate the average values for UKO, FRA, and USA
filtered_data_2 <- filtered_data %>%
  filter(country %in% c("UKO", "FRA", "USA")) %>%
  group_by(ccode1, year) %>%
  summarise(IdealPointDistance = mean(IdealPointDistance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(country = "AVG_UKO_FRA_USA") %>%
  select(ccode1, country, year, IdealPointDistance)

# Combine filtered_data and filtered_data_2, and remove rows for UKO, FRA, and USA
combined_data <- filtered_data %>%
  filter(!country %in% c("UKO", "FRA", "USA")) %>%
  bind_rows(filtered_data_2)

# Pivot the COMBINED data to a wide format
pivoted_combined_data <- combined_data %>%
  pivot_wider(names_from = country, values_from = IdealPointDistance, names_prefix = "IdealPointDistance_") %>%
  filter( !ccode1 %in% c(365, 2, 220, 200, 710)) %>%
  group_by(ccode1) %>%
  summarise(across(starts_with("IdealPointDistance_"), ~ mean(.x, na.rm = TRUE)))

# Write out the filtered dataset to a single file
write_csv(combined_data, here::here("data/un_ideal_points_avg.csv"))

#Original Pivotied data pre average
# Pivot the data to a wide format
pivoted_data <- filtered_data %>%
  pivot_wider(names_from = country, values_from = IdealPointDistance, names_prefix = "IdealPointDistance_") %>%
  filter( !ccode1 %in% c(365, 2, 220, 200, 710)) %>%
  group_by(ccode1) %>%
  summarise(across(starts_with("IdealPointDistance_"), ~ mean(.x, na.rm = TRUE)))

# Write out the pivoted dataset to a single file
write_csv(pivoted_data, here::here("data/un_ideal_points.csv"))


