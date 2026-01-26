# This script analyzes the strategic factors that predict increases in Russian influence
# in 2022-2024 compared to 2017-2021. We look at UN Ideal Point distances, characteristics
# like distance and language, and trade factors
library(countrycode)
library(modelsummary)
library(tradestatistics)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
# Read-in and clean RAI data ----------------------------------------------

df = readr::read_csv(here::here("writing", "output", "rai_4_5_24.csv")) |>
  filter(influencer=="Russia") |>
  mutate(period = case_when(
    date < "2017-01-01" ~ "2012-2016",
    date >= "2017-01-01" & date < "2021-01-01" ~ "2017-2021",
    TRUE ~ "2022-2024"
  )
  ) |>
  select(country, date, rai_idx_hp, rai_idx_dip, period, influencer)

df <- df |>
  group_by(country, period, influencer) |>
  summarize(mean_hp = mean(rai_idx_hp, na.rm = T),
            mean_dip = mean(rai_idx_dip, na.rm = T),
            .groups = "drop") |>
  arrange(country, influencer, period)


dfp <- df |>
  group_by(country) |>
  mutate(percentage_change_hp = (mean_hp / lag(mean_hp) - 1) * 100) |>
  mutate(increase_hp = mean_hp[3] - mean_hp[2]) |>
  mutate(percentage_change_dip = (mean_dip / lag(mean_dip) - 1) * 100) |>
  mutate(increase_dip = mean_dip[3] - mean_dip[2]) |>
  filter(!is.na(percentage_change_hp))

## Prepare for merge
dfp = dfp %>%
  mutate(ccode1 = countrycode(country, "country.name", "cown")) %>%
  mutate(ccode1 = case_when(country == "Serbia" ~ 345,
                            TRUE ~ ccode1))
# UN Ideal Points ---------------------------------------------------------

## UN Voting Data
dat = read_csv(here::here("data/un_ideal_points_avg.csv") ) 

df_filtered <- dat %>%
  mutate(country = case_when(
    country == "RUS" ~ "Russia",
    country == "CHN" ~ "China",
    country == "AVG_UKO_FRA_USA" ~ "West",
  )
  ) %>%
  pivot_wider(names_from = country, values_from = IdealPointDistance) %>%
  select(West, China, Russia)

# Calculate the correlation matrix
correlation_matrix <- cor(df_filtered, use = "complete.obs")

# Convert the correlation matrix into a long format
correlation_long <- as.data.frame(as.table(correlation_matrix))

# Order the labels
order_labels <- c("West", "China", "Russia")
correlation_long$Var1 <- factor(correlation_long$Var1, levels = order_labels)
correlation_long$Var2 <- factor(correlation_long$Var2, levels = order_labels)

# Visualize with ggplot2
ggplot(correlation_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1)) +
  coord_fixed() +
  labs(x = "", y = "", title = "UN Ideal Points (mean 2017-2021)")
ggsave(here::here("writing/un_ideal_points_cor.png"), height = 3, width = 4)


# Merge UN and RAI data ---------------------------------------------------

## Merge
data = left_join(dfp, dat, by = "ccode1") %>%
  filter(period == "2022-2024") %>%
  mutate(iso = countrycode(country, "country.name", "iso3c") ) %>%
  mutate(iso = case_when(country == "Kosovo" ~ "XKX",
                         TRUE ~ iso))


# Trade Data --------------------------------------------------------------

# guide = as_tibble(ots_tables)
clist = tolower(unique(data$iso))[ tolower(unique(data$iso)) %in%  ots_countries$country_iso ]

yrpc <- ots_create_tidy_data(
  years = 2019,
  reporters = clist,
  partners = "rus",
  table = "yrp"
)

dist = tradestatistics::ots_distances %>%
  filter( country2 ==  "rus") %>%
  rename(reporter_iso = country1)

tdat = left_join(yrpc, dist)

tdat = tdat %>%
  mutate(iso = toupper(reporter_iso))

data = left_join(data, tdat)



# Hard Power Models -------------------------------------------------------


models <- list()
models[['Increase (Articles)']] = lm(increase_hp ~ IdealPointDistance_USA + #IdealPointDistance_UKO + IdealPointDistance_FRA +  
                                       IdealPointDistance_RUS + IdealPointDistance_CHN , data)
models[['Increase (Articles) Some']] = lm(increase_hp ~ IdealPointDistance_USA + #IdealPointDistance_UKO + IdealPointDistance_FRA +  
                                            IdealPointDistance_RUS + IdealPointDistance_CHN + 
                                            dist , data)
models[['Increase (Articles) Full']] = lm(increase_hp ~ IdealPointDistance_UKO + IdealPointDistance_FRA +  IdealPointDistance_USA + 
                                            IdealPointDistance_RUS + IdealPointDistance_CHN +
                                            comlang_ethno , data)



modelsummary(
  models,
  # coef_map = c('(Intercept)' = "Intercept",
  #              'IdealPointDistance' = "IdealPointDistance",
  #              'trade_value_usd_imp' = 'Imports (USD)', 'trade_value_usd_exp' = 'Exports (USD)',
  #              'dist' = "Distance (km)", 'comlang_ethno' = "Language", 'contig' = "Contiguous"),
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.')


modelsummary(
  models,
  coef_map = c('(Intercept)' = "Intercept",
               'IdealPointDistance' = "IdealPointDistance",
               'trade_value_usd_imp' = 'Imports (USD)', 'trade_value_usd_exp' = 'Exports (USD)',
               'dist' = "Distance (km)", 'comlang_ethno' = "Language", 'contig' = "Contiguous"),
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.')






