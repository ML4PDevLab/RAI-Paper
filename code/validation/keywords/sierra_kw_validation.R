library(googlesheets4)
library(tidyverse)
library(gt)

sheet_url <- "https://docs.google.com/spreadsheets/d/1HDK_nn9sWIUBn5CcVFPgNDHlCtnnD2YvVaVQ4JmVvRs/edit?usp=sharing"
sheet_list <- sheet_names(sheet_url)
# Country-overall lists
country_list = sheet_list[!grepl(":", sheet_list)]
# Keyword lists
kw_list = sheet_list[grepl(":", sheet_list)]
# original_list = sheet_list[grepl(".*: RAI_keywords$", sheet_list)]
# sierra_list = sheet_list[grepl(".*: China_all_kw$", sheet_list)]
# province_list = sheet_list[grepl(".*: China_province_only$", sheet_list)]
# expanded_list = sheet_list[grepl(".*: China_china_chinese_first_two$", sheet_list)]


# Function to read a sheet and add a column for the sheet name
read_and_label_sheet <- function(sheet_name) {
  sheet_data <- read_sheet(sheet_url, sheet = sheet_name) 
  colnames(sheet_data)[1] <- "keyword"
  sheet_data <- sheet_data %>% mutate(sheet_name = sheet_name)
  return(sheet_data)
}


dat <- map_df(kw_list, read_and_label_sheet)

dat = dat %>%
  select(sheet_name, everything()) %>%
  mutate(sheet_name = gsub(".*:\\s", "", sheet_name)) %>% 
  group_by(sheet_name, keyword) %>%
  summarize(across(everything(), sum, na.rm = TRUE)) %>%
  mutate(tp_share = `TRUE`/Total) 

write.csv(dat, here::here("data/sierra_kw_fp_frequency"))



# New function ------------------------------------------------------------

# Function to read a sheet and add a column for the sheet name
read_and_label_sheet <- function(sheet_name) {
  sheet_data <- read_sheet(sheet_url, sheet = sheet_name)
  sheet_data <- sheet_data %>% mutate(sheet_name = sheet_name)
  return(sheet_data)
}

# Calculate overall false positive rate across countries ------------------

# Use purrr to read and bind all sheets
cdat <- map_df(country_list, read_and_label_sheet) %>%
  select(sheet_name, everything()) %>%
  mutate(event_type_RAI_China = as.character(event_type_RAI_China))

write_csv(cdat, here::here("data/sierra_articles_coded.csv"))

# Create a contingency table
cdat = cdat %>% mutate(kw_true_positive = factor(kw_true_positive, 
                                                 levels = c(0, 1),
                                                 labels = c("False Positive", "True Positive")))

# Original keywords -------------------------------------------------------

# Summarize the data to get frequencies for each sheet_name and kw_true_positive
original_kw <- cdat %>%
  filter(!RAI_keywords == "[]") %>%
  #filter(!event_type_RAI_China == "-999") %>%
  group_by(sheet_name, kw_true_positive) %>%
  summarize(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = kw_true_positive, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(Total = `False Positive` + `True Positive`,
         `False Positive %` = (`False Positive` / Total) * 100,
         `True Positive %` = (`True Positive` / Total) * 100) %>%
  select(sheet_name, `False Positive`, `True Positive`, `True Positive %`)

# Calculate the overall totals and percentages
total_row <- original_kw %>%
  summarise(
    sheet_name = "Total",
    `False Positive` = sum(`False Positive`, na.rm = TRUE),
    `True Positive` = sum(`True Positive`, na.rm = TRUE),
    `True Positive %` = (sum(`True Positive`, na.rm = TRUE) / sum(`False Positive` + `True Positive`, na.rm = TRUE)) * 100
  )

# Append the total row to the original data
original_kw <- bind_rows(original_kw, total_row)


# Create a gt table
gt_table <- original_kw %>%
  gt() %>%
  tab_header(
    title = "Original Keywords"
  ) %>%
  cols_label(
    sheet_name = "Sheet Name",
    `False Positive` = "False Positive",
    `True Positive` = "True Positive",
    `True Positive %` = "True Positive %"
  ) %>%
  fmt_number(
    columns = c(`True Positive %`),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small"
  )

# Print the gt table
print(gt_table)

# Sierra Keywords ---------------------------------------------------------


# Summarize the data to get frequencies for each sheet_name and kw_true_positive
sierra_kw <- cdat %>%
  filter(!Test_RAI_keywords_China_all_kw == "[]") %>%
  #filter(!event_type_RAI_China == "-999") %>%
  group_by(sheet_name, kw_true_positive) %>%
  summarize(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = kw_true_positive, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(Total = `False Positive` + `True Positive`,
         `False Positive %` = (`False Positive` / Total) * 100,
         `True Positive %` = (`True Positive` / Total) * 100) %>%
  select(sheet_name, `False Positive`, `True Positive`, `True Positive %`)

# Calculate the overall totals and percentages
total_row <- sierra_kw %>%
  summarise(
    sheet_name = "Total",
    `False Positive` = sum(`False Positive`, na.rm = TRUE),
    `True Positive` = sum(`True Positive`, na.rm = TRUE),
    `True Positive %` = (sum(`True Positive`, na.rm = TRUE) / sum(`False Positive` + `True Positive`, na.rm = TRUE)) * 100
  )

# Append the total row to the original data
sierra_kw <- bind_rows(sierra_kw, total_row)


# Create a gt table
gt_table <- sierra_kw %>%
  gt() %>%
  tab_header(
    title = "All Sierra Keywords"
  ) %>%
  cols_label(
    sheet_name = "Sheet Name",
    `False Positive` = "False Positive",
    `True Positive` = "True Positive",
    `True Positive %` = "True Positive %"
  ) %>%
  fmt_number(
    columns = c(`True Positive %`),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small"
  )

# Print the gt table
print(gt_table)

# Sierra Modified Keywords ------------------------------------------------

# Summarize the data to get frequencies for each sheet_name and kw_true_positive
sierra_kw <- cdat %>%
  filter(!Test_RAI_keywords_China_province_only == "[]") %>%
  #filter(!event_type_RAI_China == "-999") %>%
  group_by(sheet_name, kw_true_positive) %>%
  summarize(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = kw_true_positive, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(Total = `False Positive` + `True Positive`,
         `False Positive %` = (`False Positive` / Total) * 100,
         `True Positive %` = (`True Positive` / Total) * 100) %>%
  select(sheet_name, `False Positive`, `True Positive`, `True Positive %`)

# Calculate the overall totals and percentages
total_row <- sierra_kw %>%
  summarise(
    sheet_name = "Total",
    `False Positive` = sum(`False Positive`, na.rm = TRUE),
    `True Positive` = sum(`True Positive`, na.rm = TRUE),
    `True Positive %` = (sum(`True Positive`, na.rm = TRUE) / sum(`False Positive` + `True Positive`, na.rm = TRUE)) * 100
  )

# Append the total row to the original data
sierra_kw <- bind_rows(sierra_kw, total_row)


# Create a gt table
gt_table <- sierra_kw %>%
  gt() %>%
  tab_header(
    title = "Sierra Modified Keywords"
  ) %>%
  cols_label(
    sheet_name = "Sheet Name",
    `False Positive` = "False Positive",
    `True Positive` = "True Positive",
    `True Positive %` = "True Positive %"
  ) %>%
  fmt_number(
    columns = c(`True Positive %`),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small"
  )

# Print the gt table
print(gt_table)

# Expanded All Sierra Keywords --------------------------------------------

# Summarize the data to get frequencies for each sheet_name and kw_true_positive
sierra_kw <- cdat %>%
  filter(!Test_RAI_keywords_China_china_chinese_first_two == "[]") %>%
  #filter(!event_type_RAI_China == "-999") %>%
  group_by(sheet_name, kw_true_positive) %>%
  summarize(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = kw_true_positive, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(Total = `False Positive` + `True Positive`,
         `False Positive %` = (`False Positive` / Total) * 100,
         `True Positive %` = (`True Positive` / Total) * 100) %>%
  select(sheet_name, `False Positive`, `True Positive`, `True Positive %`)

# Calculate the overall totals and percentages
total_row <- sierra_kw %>%
  summarise(
    sheet_name = "Total",
    `False Positive` = sum(`False Positive`, na.rm = TRUE),
    `True Positive` = sum(`True Positive`, na.rm = TRUE),
    `True Positive %` = (sum(`True Positive`, na.rm = TRUE) / sum(`False Positive` + `True Positive`, na.rm = TRUE)) * 100
  )

# Append the total row to the original data
sierra_kw <- bind_rows(sierra_kw, total_row)


# Create a gt table
gt_table <- sierra_kw %>%
  gt() %>%
  tab_header(
    title = "Expanded All Sierra Keywords"
  ) %>%
  cols_label(
    sheet_name = "Sheet Name",
    `False Positive` = "False Positive",
    `True Positive` = "True Positive",
    `True Positive %` = "True Positive %"
  ) %>%
  fmt_number(
    columns = c(`True Positive %`),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small"
  )

# Print the gt table
print(gt_table)
