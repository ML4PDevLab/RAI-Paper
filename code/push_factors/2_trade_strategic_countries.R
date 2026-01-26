# This script uses data from `trade_critical_commodities.R` to identify strategically important countries 
# Step 1: Identify countries that import/export critical commodities on which Russia is reliant
# Step 2: Visualize the distribution
library(tradestatistics)
library(tidyverse)


# Step 2: Identify the reliance commodities and match to countries --------
dat = read_csv(here::here("data/trade_dat_headings.csv"))

# Get COMTRADE country list
c_list = tradestatistics::ots_countries

# Get MLP country list
countries = readr::read_csv(here::here("writing", "output", "rai_8_30_24.csv")) %>%
  select(country) %>%
  distinct()

# Match names across datasets
countries <- countries %>%
  mutate(country_name_english = case_when(
    country == "Moldova"  ~ "Rep. of Moldova",
    country == "Tanzania" ~ "United Rep. of Tanzania",
    country == "Macedonia" ~ "TFYR of Macedonia",
    country == "DR Congo" ~ "Dem. Rep. of the Congo",
    country == "Dominican Republic" ~ "Dominican Rep.",
    country == "Timor Leste" ~ "Timor-Leste",
    TRUE ~ country
  ))



# Merge
# countries$country[!countries$country %in% ots_countries$country_name_english]
countries = left_join(countries, c_list)


# Pull relevant data from API ---------------------------------------------

## Import Reliance: Things that Russia Imports more than it exports by a lot
## this signifies an inability to produce products domestically and a need to import
## we're interested in identifying countries that export these products
critical_imports = dat %>% filter(import_reliance == 1)

## Export Reliance: Things that Russia exports a lot of
## this signifies a reliance on exports for forex
## we're interested in identifying countries that import these products
critical_exports = dat %>% filter(export_reliance == 1)

cs = countries %>% filter(country != "Kosovo") %>% select(country_iso) %>% pull

# Create a tidy dataset of trade statistics
tdat <- ots_create_tidy_data(
  years = 2015:2019,          # Specify the range of years to include in the dataset
  reporters = c(cs ),    # Specify the reporting country
  partners = c("all"),        # Include all partner countries
  commodities = c("all"),
  table = "yrc"              # Specify the table to use (yearly, reporter, partner, commodity)
)

tdat = tdat %>%
  filter(section_code != "99")

adat = tdat %>%
  mutate(heading_code = substr(commodity_code, 1, 4)) %>%
  group_by(heading_code, reporter_iso) %>%
  summarize(
    trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE),
    trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE)
  ) 

# cs[!cs %in% unique(adat$reporter_iso)]

# Countries exporting critical imports ------------------------------------

expt = adat %>%
  filter(heading_code %in% critical_imports$heading_code) %>%
  filter(trade_value_usd_exp > 0) %>%
  rename(country_iso = reporter_iso,
         trade_value_usd_imp_rep = trade_value_usd_imp,
         trade_value_usd_exp_rep = trade_value_usd_exp) %>%
  left_join( countries) %>%
  left_join( critical_imports)

expt = expt %>%
  mutate(trade_value_usd_exp_rep_ratio = trade_value_usd_exp_rep / trade_value_usd_imp) 

expt = expt %>%
  group_by(country) %>%
  summarise(
    continent_name_english = first(continent_name_english),
    trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE),
    trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE),
    trade_value_usd_imp_rep = sum(trade_value_usd_imp_rep, na.rm = TRUE),
    trade_value_usd_exp_rep = sum(trade_value_usd_exp_rep, na.rm = TRUE),
    export_reliance_headings = n(),
    trade_value_usd_exp_rep_ratio = max(trade_value_usd_exp_rep_ratio),
    trade_value_usd_exp_rep_ratio_heading = heading_name[which.max(trade_value_usd_exp_rep_ratio)]
  )

expt = expt %>%
  mutate(import_reliance_exporter = case_when(
    export_reliance_headings >= 70 & trade_value_usd_exp_rep_ratio > 1 ~ 1,
    TRUE ~ 0
  ))

# Calculate the number of countries in each continent where import_reliance_exporter == 1
counts <- expt %>%
  filter(import_reliance_exporter == 1) %>%
  group_by(continent_name_english) %>%
  summarise(count = n_distinct(country))

# Create a new column in the original dataset with the continent name and the count
expt <- expt %>%
  left_join(counts, by = "continent_name_english") %>%
  mutate(continent_with_count = paste0(continent_name_english, " (", count, ")"))


ggplot(expt , aes(x = trade_value_usd_exp_rep_ratio, y = export_reliance_headings, color = continent_with_count)) +
  geom_jitter(alpha = .7, width = 0, height = 0.1) +
  ggrepel::geom_text_repel(data = expt %>%
                             filter(import_reliance_exporter == 1),
                           min.segment.length = unit(0, 'lines'), 
                           force = 1,
                           max.overlaps = Inf,
                           aes(label = country), size = 2, color = "black") +  # Add text labels for the top 5 points
  scale_x_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(75, NA)) +  # Ensure y-axis has whole numbers
  labs(title = "Exporters of Russian Import Reliance Commodities",
       subtitle = str_wrap("Labels are countries with at least one commodity where the ratio of country's exports to Russian imports exceeds 100%. Excludes countries with fewer than 75 exported commodities. Legend reports count of labeled countries by continent"),
       x = "Ratio of country's exports to Russian imports for highest ratio commodity",
       y = "Count of import reliance commodities exported (jittered)",
       color = NULL) +  # Add legend title for color
  theme_bw() +
  theme(legend.position =  c(0.65, 0.6),
        legend.text = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 8))

ggsave(here::here("writing/import_reliance_heading_exporters.png"), width = 7, height = 5)

# Countries that import critical exports ----------------------------------

# Identify exports and merge
impt = adat %>%
  filter(trade_value_usd_imp > 0) %>%
  rename(country_iso = reporter_iso,
         trade_value_usd_imp_rep = trade_value_usd_imp,
         trade_value_usd_exp_rep = trade_value_usd_exp) %>%
  left_join( countries) %>%
  left_join( critical_exports)

# Calculate import reliance
impt = impt %>%
  mutate(trade_value_usd_tbal_rep = trade_value_usd_exp_rep - trade_value_usd_imp_rep, # What is exported more than imported?
         import_factor_rep = trade_value_usd_imp_rep / (trade_value_usd_imp_rep + trade_value_usd_exp_rep),  # Calculate import factor (share of trade from imports)
         trade_share_rep = (trade_value_usd_imp_rep + trade_value_usd_exp_rep) / sum(trade_value_usd_imp_rep + trade_value_usd_exp_rep, na.rm = TRUE),  # Calculate trade share (divide total trade for each row by total trade for entire column)
         import_reliance_rep = case_when(trade_share_rep > .001 & 
                                                       import_factor_rep > .8 ~ 1, # Identify commodities on which countries are import reliant
                                            TRUE ~ 0)
         ) %>%
  filter(heading_code %in% critical_exports$heading_code) %>%
  filter(import_reliance_rep == 1)
  
# Identify which countries have heavy import reliance
impt = impt %>%
  group_by(country) %>%
  summarise(
    continent_name_english = first(continent_name_english),
    import_reliance_headings = n(), # count of Russia's export-reliance commodities that country is import reliant on  
    trade_value_usd_tbal_rep = min(trade_value_usd_tbal_rep), # Largest negative trade balance
    trade_share_rep = max(trade_share_rep), # Max value of trade share across commodities
    import_reliance_rep_value = sum( trade_value_usd_imp_rep), # Value of trade for import reliance commodity
    import_reliance_rep_factor = max( import_factor_rep), # Max value of import_factor for import reliance commodity
    import_reliance_rep_value_heading = heading_name[which.max(trade_value_usd_imp_rep)]
    
  )


# Each row is a commodity imported by rep and on which Rus is export reliant

impt = impt %>%
  mutate(export_reliance_import_reliant = case_when(import_reliance_rep_value > 15000000000 & 
                                                      import_reliance_headings > 10 ~ 1,
                                                    TRUE ~ 0))

# Calculate the number of countries in each continent where import_reliance_exporter == 1
counts <- impt %>%
  filter(export_reliance_import_reliant == 1) %>%
  group_by(continent_name_english) %>%
  summarise(count = n_distinct(country))

# Create a new column in the original dataset with the continent name and the count
impt <- impt %>%
  left_join(counts, by = "continent_name_english") %>%
  mutate(continent_with_count = paste0(continent_name_english, " (", count, ")"))


ggplot(impt , aes(x = import_reliance_headings, y = import_reliance_rep_value, color = continent_with_count)) +
  geom_point(alpha = 0.5) +
  ggrepel::geom_text_repel(data = impt %>%
                             filter(export_reliance_import_reliant == 1),
                           min.segment.length = unit(0, 'lines'),
                           force = 1,
                           max.overlaps = Inf,
                           aes(label = country), size = 2, color = "black") +  # Add text labels for the top 5 points
  scale_y_continuous(labels = function(x) {paste0(x / 1000000000, "B")}) + # normalize by billions
  labs(title = "Import Reliance on Russian Export Reliance Commodities",
       subtitle = str_wrap("Labels are countries with import reliance on at least 10 commodities on which Russia is export reliant and at least 15B in value."),
       x = "Count of Russia's export-reliance commodities that country is import reliant on",
       y = "Imports for Russia's export-reliance commodities\n that country is import reliant on (2015-2019)",  # Add legend title for color
       color = NULL) +
  theme_bw() +
  theme(legend.position =  c(0.65, 0.6),
        legend.text = element_text(size = 8),
        plot.subtitle = element_text(size = 8))

ggsave(here::here("writing/export_reliance_heading_importers.png"), width = 7, height = 5)

# Merge measures of export and import reliance ----------------------------


## Clean-up impt columns
impt <- impt %>%
  # Select columns you want to keep and rename with prefix
  select(country, continent_name_english, 
         import_reliance_headings,
         trade_value_usd_tbal_rep,
         trade_share_rep,
         import_reliance_rep_value,
         import_reliance_rep_factor,
         import_reliance_rep_value_heading,
         export_reliance_import_reliant) %>%
  # Add prefix to specific columns
  rename_with(~ paste0("impt_", .), 
              import_reliance_headings:export_reliance_import_reliant)


## Clean-up expt columns
expt = expt %>%
  # Select columns you want to keep and rename with prefix
  select(country, continent_name_english, 
         trade_value_usd_exp_rep,
         export_reliance_headings,
         trade_value_usd_exp_rep_ratio,
         trade_value_usd_exp_rep_ratio_heading,
         import_reliance_exporter) %>%
  # Add prefix to specific columns
  rename_with(~ paste0("expt_", .), 
              trade_value_usd_exp_rep:import_reliance_exporter)

  
## Merge dataframes
data = left_join(impt, expt, by = c("country", "continent_name_english"))

table(data$expt_import_reliance_exporter, data$impt_export_reliance_import_reliant)
cor(data$expt_import_reliance_exporter, data$impt_export_reliance_import_reliant)

write_csv(data, here::here("data/strategic_countries.csv"))

