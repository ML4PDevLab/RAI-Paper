# This script pulls Russian imports/exports data from COMTRADE via the `tradestatistics` API at 3 levels of granularity
# Step 1: Identify commodities on which Russia is reliant (both imports and exports)
# Step 2: Visualize the distribution
library(tradestatistics)
library(tidyverse)

# Read-in trade data via API ----------------------------------------------

# Create a tidy dataset of trade statistics
tdat <- ots_create_tidy_data(
  years = 2015:2019,          # Specify the range of years to include in the dataset
  reporters = c("Russia"),    # Specify the reporting country
  partners = c("all"),        # Include all partner countries
  commodities = c("all"),     # Include all commodities
  table = "yrpc"              # Specify the table to use (yearly, reporter, partner, commodity)
)


# Trade values function ---------------------------------------------------

calculate_trade_values <- function(data, grouping_type) {
  # Provided list
  section_names_list <- c("Animals", "Vegetables", "Fats and oils", "Prepared food", "Minerals",
                          "Chemicals", "Plastics", "Leathers", "Wood", "Wood pulp", "Textiles",
                          "Footwear", "Stone", "Precious metals", "Base metals", "Machinery",
                          "Vehicles", "Specialized instruments", "Arms", "Manufactured articles", "Art", "Other")
  
  # Create a dataframe from the list
  section_names_df <- data.frame(section_abbrev = section_names_list, stringsAsFactors = FALSE)
  
  section_dict <- data %>%
    select(section_code, section_name) %>%
    distinct()
  
  section_dict <- cbind(section_dict, section_names_df)
  
  # Determine the grouping variable and create the dictionary
  if (grouping_type == "commodity") {
    grouping_var <- "commodity_code"
    code_dict <- data %>%
      select(commodity_code, commodity_name, section_code, section_name) %>%
      distinct()
    
    code_dict <- left_join(code_dict, section_dict)
    
  } else if (grouping_type == "heading") {
    
    data = data %>%
      mutate(heading_code = substr(commodity_code, 1, 4))
    
    
    # Take first 4 of commodity_code
    # Then merge with section_dict
    heading_dict <- data %>%
      mutate(heading_code = substr(commodity_code, 1, 4)) %>%
      select(heading_code, section_code, section_name) %>%
      distinct()
    
    heading_dict = left_join(heading_dict, section_dict)
    
    
    grouping_var <- "heading_code"
    code_dict <- read_csv("data/hs12_codes.csv") %>%
      filter(Level == 4 ) %>%
      select(Code, Description) %>%
      rename(heading_code = Code, heading_name = Description)
    
    
    code_dict <- left_join(code_dict, heading_dict)
    
    
  } else if (grouping_type == "section") {
    grouping_var <- "section_code"
    code_dict <- section_dict
    
  } else {
    stop("Invalid grouping_type. Please specify either 'commodity' or 'section'.")
  }
  
  # Perform the calculations
  result <- data %>%
    ots_gdp_deflator_adjustment(reference_year = 2015) %>%
    filter(!is.na( section_name ) ) %>% # "Commodities not specified according to kind"
    group_by(across(all_of(grouping_var))) %>%
    summarize(
      trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE),
      trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE)
    ) %>%
    left_join(code_dict, by = grouping_var) %>%
    mutate(
      trade_value_usd_tbal = trade_value_usd_exp - trade_value_usd_imp, # What is exported more than imported?
      export_rank = dense_rank(desc(trade_value_usd_exp)),  # Rank exports in descending order
      import_rank = dense_rank(desc(trade_value_usd_imp)),  # Rank imports in descending order
      trade_balance_rank = dense_rank(trade_value_usd_tbal),  # Rank trade balance (those where imports are much higher)
      import_factor = trade_value_usd_imp / (trade_value_usd_imp + trade_value_usd_exp),  # Calculate import factor (share of trade from imports)
      export_factor = trade_value_usd_exp / (trade_value_usd_imp + trade_value_usd_exp),  # Calculate export factor (share of trade from exports)
      trade_share = (trade_value_usd_imp + trade_value_usd_exp) / sum(trade_value_usd_imp + trade_value_usd_exp, na.rm = TRUE),  # Calculate trade share (divide total trade for each row by total trade for entire column)
    )
  
  return(result)
}


# Apply functions to generate dataframes ----------------------------------

# dat_c <- calculate_trade_values(tdat, "commodity")
dat_h <- calculate_trade_values(tdat, "heading")
dat_s <- calculate_trade_values(tdat, "section")


# Calculate import reliance -----------------------------------------------

## Import Reliance: Things that Russia Imports more than it exports by a lot
## this signifies an inability to produce products domestically and a need to import
## we're interested in identifying countries that export these products

## Section plot
ggplot(dat_s, aes(x = import_factor, y = trade_share )) +
  geom_point(aes(color = trade_value_usd_tbal < 0 )) +  # Color points based on combined condition
  scale_color_manual(values = c("black", "red")) +  # Define colors
  ggrepel::geom_text_repel(aes(label = section_abbrev), size = 3) +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  scale_x_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Import Reliance for COMTRADE HS2 Sections",
       subtitle = str_wrap("Red dots capture commodity sections where imports exceed exports"),
       x = "Imports share of commodity trade",
       y = "Commodity share of total trade") +  # Add legend title for color
  theme_bw() +
  theme(legend.position = "none")

ggsave(here::here("writing/import_reliance_section.png"), width = 7, height = 5)


## Heading plot

# Define import reliance
dat_h = dat_h %>%
  mutate(import_reliance = case_when(trade_share > .001 & import_factor > .8  ~ 1,
                                     TRUE ~ 0))

# Calculate counts for each group
counts <- dat_h %>%
  filter(import_reliance == 1) %>%
  group_by(section_abbrev) %>%
  summarise(count = n())

# Join counts back to the original data
dat_h_with_counts <- dat_h %>%
  filter(import_reliance == 1) %>%
  left_join(counts, by = "section_abbrev")

# Create new labels with counts
labels_with_counts <- counts %>%
  mutate(label = paste0(section_abbrev, " (n=", count, ")")) %>%
  pull(label)

# Create a named vector for the new labels
names(labels_with_counts) <- counts$section_abbrev

# Perform a random sample of 1 row from each unique value of section_abbrev
sampled_data <- dat_h %>%
  filter(import_reliance == 1) %>%
  arrange(desc(trade_share)) %>%
  mutate(heading_name = str_extract(heading_name, "^[^[:punct:]&&[^-]]+")) %>%
  mutate(heading_name =  str_replace(heading_name, " and.*", "")) %>%
  mutate(heading_name = str_wrap(heading_name, width = 15) )

# Sample the top import from each section, excluding the top 6
sampled = sampled_data %>%
  tail(-6) %>%
  group_by(section_abbrev) %>%
  slice_sample(n = 1)

# Identify the top 6 points, which are outliers based on the y-axis values
top_6 <- sampled_data %>%
  head(6) 

top_6 = bind_rows(top_6, sampled)

ggplot(dat_h %>%
         filter(import_reliance == 1), aes(x = trade_balance_rank, y = trade_share, color = section_abbrev)) +
  geom_point(alpha = 0.5) +
  ggrepel::geom_text_repel(data = top_6, min.segment.length = unit(0, 'lines'), 
                           aes(label = heading_name), size = 2, color = "black") +  # Add text labels for the top 5 points
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Import Reliance for COMTRADE HS4 Headings",
       subtitle = str_wrap("More than 80% trade in heading from imports and header greater than 0.1% of total trade. Legend reports count of commodity headings in each section."),
       x = "Rank of import's share of commodity trade",
       y = "Commodity share of total trade",  # Add legend title for color
       color = NULL) +
  theme_bw() +
  scale_color_manual(values = scales::hue_pal()(length(unique(dat_h_with_counts$section_abbrev))),
                     labels = labels_with_counts) +
  guides(colour = guide_legend(ncol = 2)) +
  theme(legend.position =  c(0.65, 0.6),
        legend.text = element_text(size = 5),
        plot.subtitle = element_text(size = 8))

ggsave(here::here("writing/import_reliance_heading.png"), width = 7, height = 5)

# Calculate export reliance -----------------------------------------------

## Export Reliance: Things that Russia exports a lot of
## this signifies a reliance on exports for forex
## we're interested in identifying countries that import these products

## Section plot
ggplot(dat_s, aes(x = export_rank, y = trade_share )) +
  geom_point(aes(color = trade_value_usd_tbal > 0 )) +  # Color points based on combined condition
  scale_color_manual(values = c("black", "red")) +  # Define colors
  ggrepel::geom_text_repel(aes(label = section_abbrev), size = 3) +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Export Reliance for COMTRADE HS2 Sections",
       subtitle = str_wrap("Red dots capture commodity sections where exports exceed imports"),
       x = "Rank of export's share of commodity trade",
       y = "Commodity share of total trade") +  # Add legend title for color
  theme_bw() +
  theme(legend.position = "none")

ggsave(here::here("writing/export_reliance_section.png"), width = 7, height = 5)


## Heading plot
dat_h = dat_h %>%
  mutate(export_reliance = case_when(trade_share > .001 & export_rank < 100 ~ 1,
                                     TRUE ~ 0) )

# Calculate counts for each group
counts <- dat_h %>%
  filter(export_reliance == 1) %>%
  group_by(section_abbrev) %>%
  summarise(count = n())

# Join counts back to the original data
dat_h_with_counts <- dat_h %>%
  filter(export_reliance == 1) %>%
  left_join(counts, by = "section_abbrev")

# Create new labels with counts
labels_with_counts <- counts %>%
  mutate(label = paste0(section_abbrev, " (n=", count, ")")) %>%
  pull(label)

# Create a named vector for the new labels
names(labels_with_counts) <- counts$section_abbrev

# Perform a random sample of 1 row from each unique value of section_abbrev
sampled_data <- dat_h %>%
  filter(export_reliance == 1) %>%
  arrange(desc(trade_share)) %>%
  mutate(heading_name = str_extract(heading_name, "^[^[:punct:]&&[^-]]+")) %>%
  mutate(heading_name = str_replace(heading_name, " and.*", "")) %>%
  mutate(heading_name = str_replace(heading_name, " not .*", "")) %>%
  mutate(heading_name = str_replace(heading_name, " or .*", "")) %>%
  mutate(heading_name = str_wrap(heading_name, width = 15) )

# Sample the top import from each section, excluding the top 6
sampled = sampled_data %>%
  tail(-6) %>%
  group_by(section_abbrev) %>%
  slice_sample(n = 1)

# Identify the top 6 points, which are outliers based on the y-axis values
top_6 <- sampled_data %>%
  head(6) 

top_6 = bind_rows(top_6, sampled)

ggplot(dat_h %>%
         filter(export_reliance == 1), aes(x = export_rank, y = trade_share, color = section_abbrev)) +
  geom_point(alpha = 0.5) +
  ggrepel::geom_text_repel(data = top_6, min.segment.length = unit(0, 'lines'),
                           aes(label = heading_name), size = 2, color = "black") +  # Add text labels for the top 5 points
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Export Reliance for COMTRADE HS4 Headings",
       subtitle = str_wrap("Header in top 100 exports by value and header greater than 0.5% of total trade. Legend reports count of commodity headings in each section."),
       x = "Rank of export's share of commodity trade",
       y = "Commodity share of total trade",  # Add legend title for color
       color = NULL) +
  theme_bw() +
  scale_color_manual(values = scales::hue_pal()(length(unique(dat_h_with_counts$section_abbrev))),
                     labels = labels_with_counts) +
  guides(colour = guide_legend(ncol = 2)) +
  theme(legend.position =  c(0.65, 0.6),
        legend.text = element_text(size = 5),
        plot.subtitle = element_text(size = 8))

ggsave(here::here("writing/export_reliance_heading.png"), width = 7, height = 5)

write_csv(dat_h, here::here("data/trade_dat_headings.csv"))

