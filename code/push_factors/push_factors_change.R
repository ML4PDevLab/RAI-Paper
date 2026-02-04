# This script analyzes the strategic factors that predict increases in Russian influence
# in 2022-2024 compared to 2017-2021. We look at UN Ideal Point distances, characteristics
# like distance and language, and trade factors
library(countrycode)
library(modelsummary)
library(tradestatistics)
library(ggplot2)
library(dplyr)
library(readr)
library(kableExtra)

# Read-in and clean RAI data ----------------------------------------------
df = readr::read_csv(here::here("writing", "output", "rai_latest.csv")) |>
  filter(influencer=="Russia") |>
  mutate(period = case_when(
    date < "2017-01-01" ~ "2012-2016",
    date >= "2017-01-01" & date < "2022-01-01" ~ "2017-2021",
    TRUE ~ "2022-2024"
  )
  ) |>
  select(country, date, rai_idx_total, rai_idx_bal, rai_idx_di, rai_idx_dip, rai_idx_ep, rai_idx_hp, rai_idx_sp, period, influencer)


## Find the average value of outcomes in each period
df <- df |>
  group_by(country, period, influencer) |>
  summarize(mean_total = mean(rai_idx_total, na.rm = T),
            mean_bal = mean(rai_idx_bal, na.rm = T),
            mean_di = mean(rai_idx_di, na.rm = T),
            mean_dip = mean(rai_idx_dip, na.rm = T),
            mean_ep = mean(rai_idx_ep, na.rm = T),
            mean_hp = mean(rai_idx_hp, na.rm = T),
            mean_sp = mean(rai_idx_sp, na.rm = T),
            .groups = "drop") |>
  arrange(country, influencer, period)


## Calculate percent change and change
dfp <- df |>
  group_by(country) |>
  mutate(percentage_change_total = (mean_total / lag(mean_total) - 1) * 100) |>
  mutate(increase_total = mean_total[3] - mean_total[2]) |>
  mutate(percentage_change_bal = (mean_bal / lag(mean_bal) - 1) * 100) |>
  mutate(increase_bal = mean_bal[3] - mean_bal[2]) |>
  mutate(percentage_change_di = (mean_di / lag(mean_di) - 1) * 100) |>
  mutate(increase_di = mean_di[3] - mean_di[2]) |>
  mutate(percentage_change_dip = (mean_dip / lag(mean_dip) - 1) * 100) |>
  mutate(increase_dip = mean_dip[3] - mean_dip[2]) |>
  mutate(percentage_change_ep = (mean_ep / lag(mean_ep) - 1) * 100) |>
  mutate(increase_ep = mean_ep[3] - mean_ep[2]) |>
  mutate(percentage_change_hp = (mean_hp / lag(mean_hp) - 1) * 100) |>
  mutate(increase_hp = mean_hp[3] - mean_hp[2]) |>
  mutate(percentage_change_sp = (mean_sp / lag(mean_sp) - 1) * 100) |>
  mutate(increase_sp = mean_sp[3] - mean_sp[2]) |>
  filter(!is.na(percentage_change_hp))

## Prepare for merge
dfp = dfp %>%
  filter(period == "2022-2024")

## Prepare for merge
dfp = dfp %>%
  mutate(ccode1 = countrycode(country, "country.name", "cown")) %>%
  mutate(ccode1 = case_when(country == "Serbia" ~ 345,
                            TRUE ~ ccode1))

# Merge with trade data and UN data ---------------------------------------

## Read-in strategic countries (`trade_strategic_countries.R`)
sc <- readr::read_csv(here::here("data/strategic_countries.csv")) %>%
  left_join(dfp)

## Read-in UN Ideal Points (`un_ideal_points.R`)
unip <- readr::read_csv(here::here("data/un_ideal_points_avg.csv")) %>%
  group_by(ccode1, country) %>%
  summarise(IdealPointDistance = mean(IdealPointDistance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = "2017-2021") %>%
  select(ccode1, country, year, IdealPointDistance) %>%
  rename(major_power = country)

## Pivot unit wider
unip = unip %>%
  tidyr::pivot_wider(names_from = major_power, 
              values_from = IdealPointDistance, 
              names_prefix = "idp_") %>%
  rename(idp_UUF = idp_AVG_UKO_FRA_USA)

## Read-in distance and cultural data
dist = tradestatistics::ots_distances %>%
  # Filter rows where either country1 or country2 is "rus"
  filter(country1 == "rus" | country2 == "rus") %>%
  # Create a new column that keeps the value from the column that is not "rus"
  mutate(new_country = if_else(country1 == "rus", country2, country1)) %>%
  select( -country2, -country1) %>%
  # Can't find COD or SRB, so replace values with nearby country
  mutate(new_country = case_when(new_country == "cog" ~ "cod",
                                 new_country == "bih" ~ "srb",
                                 TRUE ~ new_country) ) %>%
  rename(reporter_iso = new_country) %>%
  mutate(ccode1 = countrycode(reporter_iso, "iso3c", "cown")) %>%
  mutate(ccode1 = case_when(reporter_iso == "srb" ~ 345,
                            TRUE ~ ccode1))


## Merge all datasets
dat <- sc %>%
  left_join(unip, by = "ccode1") %>%
  left_join(dist, by = "ccode1") 

# unique(df$country)[!unique(df$country) %in% dat$country]

# Hard Power Models -------------------------------------------------------

# Define the covariates
covariates <- list(
  expt = "impt_export_reliance_import_reliant",
  impt = "expt_import_reliance_exporter",
  wr = "idp_UUF + idp_RUS",
  chn = "idp_CHN",
  dist = "dist"
)

# Define the outcome variables
outcomes <- c("increase_hp", "percentage_change_hp", "increase_dip", "percentage_change_dip", "increase_total", "percentage_change_total")

# Define the names of the models
model_names <- c('Hard Power Increase (Articles)', 'Hard Power Increase (% Change)', 'Diplomacy Increase (Articles)', 'Diplomacy Increase (% Change)', "Total Increase (Articles)", "Total Increase (% Change)")

# Initialize an empty list to store models
models <- list()

# Loop through outcomes and model names to create the models
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  for (cov in names(covariates)) {
    formula <- as.formula(paste(outcome, "~", covariates[[cov]]))
    model_name <- paste(model_names[i], cov, sep = " - ")
    models[[model_name]] <- lm(formula, data = dat)
  }
}

for (outcome in model_names) {
  modelsummary(
    models[ grepl(outcome, names(models), fixed = TRUE ) ],
    coef_map = c('(Intercept)' = "Intercept",
                 'impt_export_reliance_import_reliant' = "Importer",
                 'expt_import_reliance_exporter' = 'Exporter', 
                 'idp_UUF' = "UNIPD (West)",
                 'idp_CHN' = "UNIPD (China)",
                 'idp_RUS' = "UNIPD (Russia)",
                 'dist' = "Distance (km)"),
    col.names = NULL,  # This removes the column titles
    estimate  = "{estimate}{stars} ({std.error})",
    statistic = NULL,
    gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
    output = here::here("writing", paste0(gsub("\\s", "_", gsub("\\(|\\)|%\\s","",outcome)), ".png"))
  )
}
