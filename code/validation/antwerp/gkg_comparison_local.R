library(dplyr)
library(here)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(readxl)
library(countrycode)

# Read-in RAI data
#load(here("data", "rai_2024-06-11"))
rai = read.csv(here("data", "civic_space.csv")) %>%
  mutate(chn_norm = rowSums(select(., starts_with("chn_") & ends_with("Norm")), na.rm = TRUE)) %>%
  mutate(chn_total = rowSums(select(., starts_with("chn_") & !ends_with("Norm")), na.rm = TRUE)) %>%
  mutate(rus_norm = rowSums(select(., starts_with("rus_") & ends_with("Norm")), na.rm = TRUE)) %>%
  mutate(rus_total = rowSums(select(., starts_with("rus_") & !ends_with("Norm")), na.rm = TRUE)) %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d') ) %>%
  select(country, date, ends_with("_norm"), ends_with("_total"))


# Function to read and process each dataset
process_gkg_data <- function(file_path, prefix) {
  read_excel(file_path) %>%
    mutate(Country = case_when(
      Country == "Democratic Republic of the Congo" ~ "DR Congo",
      Country == "North Macedonia" ~ "Macedonia",
      TRUE ~ Country  # Keep other countries unchanged
    )) %>% 
    pivot_longer(
      cols = matches("^[A-Za-z]{3}-\\d{2}$"),
      names_to = "date",
      values_to = "value"
    ) %>%
    pivot_wider(
      id_cols = c(Country, date),
      names_from = Column2,
      values_from = value
    ) %>%
    rename_with(~ paste0(prefix, "gkg_total"), ends_with("Weighted Count")) %>%
    rename_with(~ paste0(prefix, "gkg_norm"), ends_with("as % of all dyads recorded that month")) %>%
    rename(country = Country) %>%
    select(1:4)
  
}

# File paths
file_path_chn <- here("data", "wetransfer_monthly-data_2024-05-18_0927", 
                      "FAI_GKG (12M) - China-Country2 dyads - only media of dyad countries.xlsx")
file_path_rus <- here("data", "wetransfer_monthly-data_2024-05-18_0927", 
                      "FAI_GKG (12M) - Russia-Country2 dyads - only media of dyad countries.xlsx")

# Read and process both datasets
dat_chn <- process_gkg_data(file_path_chn, "chn_")
dat_rus <- process_gkg_data(file_path_rus, "rus_")

# Combine datasets
dat <- inner_join(dat_chn, dat_rus)

# Further processing on the combined dataset
dat <- dat %>%
  filter(country %in% unique(rai$country)) %>%
  mutate(date = parse_date_time(date, orders = "b-y")) %>%
  mutate(date = floor_date(date, unit = "month"))

stopifnot(unique(rai$country)[!unique(rai$country) %in% unique(dat$country)] < 1)  

# Merge with RAI data
dat = left_join(dat, rai) %>%
  filter(date < "2023-01-01")

# Assign region
dat$region <- countrycode(dat$country, "country.name", "region")

# Create correlation matrix
cm = cor(dat %>% select(3:10))

melted_corr_matrix <- melt(cm)

f_order = c("chn_gkg_total", "chn_gkg_norm", "chn_norm", "chn_total", "rus_gkg_total", "rus_gkg_norm", "rus_norm", "rus_total")
melted_corr_matrix$Var2 = factor(melted_corr_matrix$Var2, levels = f_order )
melted_corr_matrix$Var1 = factor(melted_corr_matrix$Var1, levels = f_order )



ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

ggsave(here::here("writing/output/corr_all_local.png"), height = 5, width = 7, dpi = 700)


# Facet by country --------------------------------------------------------


create_corr_data <- function(sub_data) {
  # Select only numeric columns
  numeric_data <- sub_data %>% select_if(is.numeric)
  corr_matrix <- cor(numeric_data)
  melted_corr_matrix <- melt(corr_matrix)
  melted_corr_matrix$region <- unique(sub_data$region)
  return(melted_corr_matrix)
}

# Split data by region
data_split <- split(dat, dat$region)

# Create a list of melted correlation matrices for each region
corr_data_list <- lapply(data_split, function(x) create_corr_data(x[, -ncol(x)]))

# Combine all data into one data frame
corr_data <- do.call(rbind, corr_data_list)

corr_data$region = gsub("\\d", "", row.names(corr_data))

corr_data$Var2 = factor(corr_data$Var2, levels = f_order )
corr_data$Var1 = factor(corr_data$Var1, levels = f_order )


# Plot the heatmap using facet_wrap
ggplot(data = corr_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  # geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  facet_wrap(~ region)

ggsave(here::here("writing/output/corr_facet_local.png"), height = 5, width = 7, dpi = 700)
