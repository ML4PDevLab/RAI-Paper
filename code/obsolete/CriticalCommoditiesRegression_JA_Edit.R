library(tradestatistics)
library(tidyverse)
library(dplyr)
library(estimatr)

StaCon <- readr::read_csv(here::here("data/strategic_countries.csv"))
#view(StaCon)

df = readr::read_csv(here::here("writing", "output", "rai_4_5_24.csv")) |>
  filter(influencer=="Russia") |>
  mutate(period = case_when(
    date < "2017-01-01" ~ "2012-2016",
    date >= "2017-01-01" & date < "2021-01-01" ~ "2017-2021",
    TRUE ~ "2022-2024"
  )
  ) |>
  select(country, date, rai_idx_total, rai_idx_bal, rai_idx_di, rai_idx_dip, rai_idx_ep, rai_idx_hp, rai_idx_sp, period, influencer)

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

#view(dfp)
#___

# dfp <- dfp %>%
#   left_join(dfpr %>% select(country, mean_total_rai, percentage_change, increase), 
#             by = c("country" = "country"))

StaCon <- StaCon %>%
  left_join(dfp %>% select(country, mean_hp, percentage_change_hp, increase_hp, mean_dip, percentage_change_dip, increase_dip, mean_ep, percentage_change_ep, increase_ep, mean_bal, percentage_change_bal, increase_bal, mean_sp, percentage_change_sp, increase_sp, mean_di, percentage_change_di, increase_di, mean_total, percentage_change_total, increase_total), 
            by = c("country" = "country"))

#view(StaCon)

codes <- readr::read_csv(here::here("data/UN Numerical code.csv"))
#view(codes)

names(codes)[names(codes) == 'Country name'] <- 'country'

StaCon <- StaCon %>%
  left_join(codes, by = "country")

IdealPointAverages <- readr::read_csv(here::here("data/un_ideal_points_avg.csv"))

IdealPointAverages <- IdealPointAverages %>%
  group_by(ccode1, country) %>%
  summarise(IdealPointDistance = mean(IdealPointDistance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = "2017-2021") %>%
  select(ccode1, country, year, IdealPointDistance)
  
names(StaCon)[names(StaCon) == 'COWn'] <- 'ccode1'

StaCon <- StaCon %>%
  left_join(IdealPointAverages, by = "ccode1")

StaConRus <- StaCon %>%
  filter(country.y=="RUS")

StaConChn <- StaCon %>%
  filter(country.y=="CHN")

StaConWes <- StaCon %>%
  filter(country.y=="AVG_UKO_FRA_USA")

## Regressions - Increase, Percent Change, Mean VS IRE/ERI
regression_model1X <- lm_robust(increase ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model2X <- lm_robust(percentage_change ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model3X <- lm_robust(mean_total_rai ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model4X <- lm_robust(increase_hp ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model5X <- lm_robust(percentage_change_hp ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model6X <- lm_robust(mean_hp ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model7X <- lm_robust(increase_dip ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model8X <- lm_robust(percentage_change_dip ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model9X <- lm_robust(mean_dip ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model10X <- lm_robust(increase_total ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model11X <- lm_robust(percentage_change_total ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model12X <- lm_robust(mean_total ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model13X <- lm_robust(increase_di ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model14X <- lm_robust(percentage_change_di ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model15X <- lm_robust(mean_di ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model16X <- lm_robust(increase_sp ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model17X <- lm_robust(percentage_change_sp ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model18X <- lm_robust(mean_sp ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model19X <- lm_robust(increase_ep ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model20X <- lm_robust(percentage_change_ep ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model21X <- lm_robust(mean_ep ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

regression_model22X <- lm_robust(increase_bal ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model23X <- lm_robust(percentage_change_bal ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model24X <- lm_robust(mean_bal ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)

## Regressions - IdealPoint VS IRE/ERI

regression_model25Y <- lm_robust(IdealPointDistance ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)
regression_model26Y <- lm_robust(IdealPointDistance ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaConRus)
regression_model27Y <- lm_robust(IdealPointDistance ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaConChn)
regression_model28Y <- lm_robust(IdealPointDistance ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaConWes)

## Regressions - Increase VS IdealPoint, by Country Group

regression_model29Z <- lm_robust(increase_ep ~  IdealPointDistance, data = StaCon)
regression_model30Z <- lm_robust(increase_ep ~  IdealPointDistance, data = StaConRus)
regression_model31Z <- lm_robust(increase_ep ~  IdealPointDistance, data = StaConChn)
regression_model32Z <- lm_robust(increase_ep ~  IdealPointDistance, data = StaConWes)

regression_model33Z <- lm_robust(increase_hp ~  IdealPointDistance, data = StaCon)
regression_model34Z <- lm_robust(increase_hp ~  IdealPointDistance, data = StaConRus)
regression_model35Z <- lm_robust(increase_hp ~  IdealPointDistance, data = StaConChn)
regression_model36Z <- lm_robust(increase_hp ~  IdealPointDistance, data = StaConWes)

regression_model37Z <- lm_robust(increase_sp ~  IdealPointDistance, data = StaCon)
regression_model38Z <- lm_robust(increase_sp ~  IdealPointDistance, data = StaConRus)
regression_model39Z <- lm_robust(increase_sp ~  IdealPointDistance, data = StaConChn)
regression_model40Z <- lm_robust(increase_sp ~  IdealPointDistance, data = StaConWes)

regression_model41Z <- lm_robust(increase_di ~  IdealPointDistance, data = StaCon)
regression_model42Z <- lm_robust(increase_di ~  IdealPointDistance, data = StaConRus)
regression_model43Z <- lm_robust(increase_di ~  IdealPointDistance, data = StaConChn)
regression_model44Z <- lm_robust(increase_di ~  IdealPointDistance, data = StaConWes)

regression_model45Z <- lm_robust(increase_dip ~  IdealPointDistance, data = StaCon)
regression_model46Z <- lm_robust(increase_dip ~  IdealPointDistance, data = StaConRus)
regression_model47Z <- lm_robust(increase_dip ~  IdealPointDistance, data = StaConChn)
regression_model48Z <- lm_robust(increase_dip ~  IdealPointDistance, data = StaConWes)

regression_model49Z <- lm_robust(increase_bal ~  IdealPointDistance, data = StaCon)
regression_model50Z <- lm_robust(increase_bal ~  IdealPointDistance, data = StaConRus)
regression_model51Z <- lm_robust(increase_bal ~  IdealPointDistance, data = StaConChn)
regression_model52Z <- lm_robust(increase_bal ~  IdealPointDistance, data = StaConWes)

regression_model53Z <- lm_robust(increase_total ~  IdealPointDistance, data = StaCon)
regression_model54Z <- lm_robust(increase_total ~  IdealPointDistance, data = StaConRus)
regression_model55Z <- lm_robust(increase_total ~  IdealPointDistance, data = StaConChn)
regression_model56Z <- lm_robust(increase_total ~  IdealPointDistance, data = StaConWes)

## Results - Increase, Percent Change, Mean VS IRE/ERI

summary(regression_model1X)
summary(regression_model2X)
summary(regression_model3X)
summary(regression_model4X)
summary(regression_model5X)
summary(regression_model6X)
summary(regression_model7X)
summary(regression_model8X)
summary(regression_model9X)
summary(regression_model10X)
summary(regression_model11X)
summary(regression_model12X)
summary(regression_model13X)
summary(regression_model14X)
summary(regression_model15X)
summary(regression_model16X)
summary(regression_model17X)
summary(regression_model18X)
summary(regression_model19X)
summary(regression_model20X)
summary(regression_model21X)
summary(regression_model22X)
summary(regression_model23X)
summary(regression_model24X)

## Results - IdealPoint VS IRE/ERI

summary(regression_model25Y)
summary(regression_model26Y)
summary(regression_model27Y)
summary(regression_model28Y)

## Results - Increase VS IdealPoint, by Country Group

summary(regression_model29Z)
summary(regression_model30Z)
summary(regression_model31Z)
summary(regression_model32Z)

summary(regression_model33Z)
summary(regression_model34Z)
summary(regression_model35Z)
summary(regression_model36Z)

summary(regression_model37Z)
summary(regression_model38Z)
summary(regression_model39Z)
summary(regression_model40Z)

summary(regression_model41Z)
summary(regression_model42Z)
summary(regression_model43Z)
summary(regression_model44Z)

summary(regression_model44Z)
summary(regression_model46Z)
summary(regression_model47Z)
summary(regression_model48Z)

summary(regression_model49Z)
summary(regression_model50Z)
summary(regression_model51Z)
summary(regression_model52Z)

summary(regression_model53Z)
summary(regression_model54Z)
summary(regression_model55Z)
summary(regression_model56Z)

#

median(StaConRus$IdealPointDistance)
sd(StaConRus$IdealPointDistance)

#___
summary(regression_model1M)
summary(regression_model2M)
summary(regression_model3M)
summary(regression_model4M)
summary(regression_model5M)
summary(regression_model6M)
summary(regression_model7M)
summary(regression_model8M)
summary(regression_model9M)






regression_model1I <- lm_robust(increase ~  mean_total_rai, data = StaCon)
regression_model2I <- lm_robust(increase ~  continent_name_english.x, data = StaCon)
regression_model3I <- lm_robust(increase ~  impt_import_reliance_headings, data = StaCon)
regression_model4I <- lm_robust(increase ~  impt_trade_value_usd_tbal_rep, data = StaCon)
regression_model5I <- lm_robust(increase ~  impt_trade_share_rep, data = StaCon)
regression_model6I <- lm_robust(increase ~  impt_import_reliance_rep_value, data = StaCon)
regression_model7I <- lm_robust(increase ~  impt_import_reliance_rep_factor, data = StaCon)
regression_model8I <- lm_robust(increase ~  impt_import_reliance_rep_value_heading, data = StaCon)
regression_model9I <- lm_robust(increase ~  impt_export_reliance_import_reliant, data = StaCon)
regression_model10I <- lm_robust(increase ~  expt_trade_value_usd_exp_rep, data = StaCon)
regression_model11I <- lm_robust(increase ~  expt_export_reliance_headings, data = StaCon)
regression_model12I <- lm_robust(increase ~  expt_trade_value_usd_exp_rep_ratio, data = StaCon)
regression_model13I <- lm_robust(increase ~  expt_trade_value_usd_exp_rep_ratio_heading, data = StaCon)
regression_model14I <- lm_robust(increase ~  expt_import_reliance_exporter, data = StaCon)
regression_model15I <- lm_robust(increase ~  percentage_change, data = StaCon)
regression_model16I <- lm_robust(increase ~  increase, data = StaCon)



summary(regression_model1I)
summary(regression_model2I)
summary(regression_model3I)
summary(regression_model4I)
summary(regression_model5I)
summary(regression_model6I)
summary(regression_model7I)
summary(regression_model8I)
summary(regression_model9I)
summary(regression_model10I)
summary(regression_model11I)
summary(regression_model12I)
summary(regression_model13I)
summary(regression_model14I)
summary(regression_model15I)

## Percent Change Regressions

regression_model1P <- lm_robust(percentage_change ~  mean_total_rai, data = StaCon)
regression_model2P <- lm_robust(percentage_change ~  continent_name_english.x, data = StaCon)
regression_model3P <- lm_robust(percentage_change ~  impt_import_reliance_headings, data = StaCon)
regression_model4P <- lm_robust(percentage_change ~  impt_trade_value_usd_tbal_rep, data = StaCon)
regression_model5P <- lm_robust(percentage_change ~  impt_trade_share_rep, data = StaCon)
regression_model6P <- lm_robust(percentage_change ~  impt_import_reliance_rep_value, data = StaCon)
regression_model7P <- lm_robust(percentage_change ~  impt_import_reliance_rep_factor, data = StaCon)
regression_model8P <- lm_robust(percentage_change ~  impt_import_reliance_rep_value_heading, data = StaCon)
regression_model9P <- lm_robust(percentage_change ~  impt_export_reliance_import_reliant, data = StaCon)
regression_model10P <- lm_robust(percentage_change ~  expt_trade_value_usd_exp_rep, data = StaCon)
regression_model11P <- lm_robust(percentage_change ~  expt_export_reliance_headings, data = StaCon)
regression_model12P <- lm_robust(percentage_change ~  expt_trade_value_usd_exp_rep_ratio, data = StaCon)
regression_model13P <- lm_robust(percentage_change ~  expt_trade_value_usd_exp_rep_ratio_heading, data = StaCon)
regression_model14P <- lm_robust(percentage_change ~  expt_import_reliance_exporter, data = StaCon)
regression_model15P <- lm_robust(percentage_change ~  increase, data = StaCon)
regression_model16P <- lm_robust(increase ~  increase, data = StaCon)



summary(regression_model1P)
summary(regression_model2P)
summary(regression_model3P)
summary(regression_model4P)
summary(regression_model5P)
summary(regression_model6P)
summary(regression_model7P)
summary(regression_model8P)
summary(regression_model9P)
summary(regression_model10P)
summary(regression_model11P)
summary(regression_model12P)
summary(regression_model13P)
summary(regression_model14P)
summary(regression_model15P)

## Mean Regressions

regression_model1M <- lm_robust(mean_total_rai ~  increase, data = StaCon)
regression_model2M <- lm_robust(mean_total_rai ~  continent_name_english.x, data = StaCon)
regression_model3M <- lm_robust(mean_total_rai ~  impt_import_reliance_headings, data = StaCon)
regression_model4M <- lm_robust(mean_total_rai ~  impt_trade_value_usd_tbal_rep, data = StaCon)
regression_model5M <- lm_robust(mean_total_rai ~  impt_trade_share_rep, data = StaCon)
regression_model6M <- lm_robust(mean_total_rai ~  impt_import_reliance_rep_value, data = StaCon)
regression_model7M <- lm_robust(mean_total_rai ~  impt_import_reliance_rep_factor, data = StaCon)
regression_model8M <- lm_robust(mean_total_rai ~  impt_import_reliance_rep_value_heading, data = StaCon)
regression_model9M <- lm_robust(mean_total_rai ~  impt_export_reliance_import_reliant, data = StaCon)
regression_model10M <- lm_robust(mean_total_rai ~  expt_trade_value_usd_exp_rep, data = StaCon)
regression_model11M <- lm_robust(mean_total_rai ~  expt_export_reliance_headings, data = StaCon)
regression_model12M <- lm_robust(mean_total_rai ~  expt_trade_value_usd_exp_rep_ratio, data = StaCon)
regression_model13M <- lm_robust(mean_total_rai ~  expt_trade_value_usd_exp_rep_ratio_heading, data = StaCon)
regression_model14M <- lm_robust(mean_total_rai ~  expt_import_reliance_exporter, data = StaCon)
regression_model15M <- lm_robust(mean_total_rai ~  percentage_change, data = StaCon)
regression_model16M <- lm_robust(mean_total_rai ~  increase, data = StaCon)



summary(regression_model1M)
summary(regression_model2M)
summary(regression_model3M)
summary(regression_model4M)
summary(regression_model5M)
summary(regression_model6M)
summary(regression_model7M)
summary(regression_model8M)
summary(regression_model9M)
summary(regression_model10M)
summary(regression_model11M)
summary(regression_model12M)
summary(regression_model13M)
summary(regression_model14M)
summary(regression_model15M)


## Additional Regressions

regression_model1X <- lm_robust(percentage_change ~  increase + mean_total_rai, data = StaCon)
regression_model2X <- lm_robust(percentage_change ~  impt_import_reliance_headings + impt_trade_value_usd_tbal_rep + impt_trade_share_rep, data = StaCon)
regression_model3X <- lm_robust(percentage_change ~  impt_import_reliance_headings + impt_trade_value_usd_tbal_rep + impt_trade_share_rep +impt_import_reliance_rep_value + impt_import_reliance_rep_factor + impt_export_reliance_import_reliant, data = StaCon)
regression_model4X <- lm_robust(percentage_change ~  impt_import_reliance_headings + expt_export_reliance_headings, data = StaCon)
regression_model9X <- lm_robust(mean_total_rai ~  impt_export_reliance_import_reliant + expt_import_reliance_exporter, data = StaCon)



regression_model1X <- lm_robust(percentage_change ~  increase, data = StaCon)
regression_model2X <- lm_robust(percentage_change ~  continent_name_english.x, data = StaCon)
regression_model3X <- lm_robust(percentage_change ~  impt_import_reliance_headings, data = StaCon)
regression_model4X <- lm_robust(percentage_change ~  impt_trade_value_usd_tbal_rep, data = StaCon)
regression_model5X <- lm_robust(percentage_change ~  impt_trade_share_rep, data = StaCon)
regression_model6X <- lm_robust(percentage_change ~  impt_import_reliance_rep_value, data = StaCon)
regression_model7X <- lm_robust(percentage_change ~  impt_import_reliance_rep_factor, data = StaCon)
regression_model8X <- lm_robust(percentage_change ~  impt_import_reliance_rep_value_heading, data = StaCon)
regression_model9X <- lm_robust(percentage_change ~  impt_export_reliance_import_reliant, data = StaCon)
regression_model10X <- lm_robust(percentage_change ~  expt_trade_value_usd_exp_rep, data = StaCon)
regression_model11X <- lm_robust(percentage_change ~  expt_export_reliance_headings, data = StaCon)
regression_model12X <- lm_robust(percentage_change ~  expt_trade_value_usd_exp_rep_ratio, data = StaCon)
regression_model13X <- lm_robust(percentage_change ~  expt_trade_value_usd_exp_rep_ratio_heading, data = StaCon)
regression_model14X <- lm_robust(percentage_change ~  expt_import_reliance_exporter, data = StaCon)
regression_model15X <- lm_robust(percentage_change ~  percentage_change, data = StaCon)
regression_model16X <- lm_robust(percentage_change ~  increase, data = StaCon)

median(StaCon$increase)
sd(StaCon$increase)
median(StaCon$increase_hp)
sd(StaCon$increase_hp)
median(StaCon$increase_dip)
sd(StaCon$increase_dip)
median(StaCon$increase_ep)
sd(StaCon$increase_ep)
median(StaCon$increase_di)
sd(StaCon$increase_di)
median(StaCon$increase_sp)
sd(StaCon$increase_sp)
median(StaCon$increase_total)
sd(StaCon$increase_total)
median(StaCon$increase_bal)
sd(StaCon$increase_bal)


summary(regression_model1X)
summary(regression_model2X)
summary(regression_model3X)
summary(regression_model4X)
summary(regression_model5X)
summary(regression_model6X)
summary(regression_model7X)
summary(regression_model8X)
summary(regression_model9X)
summary(regression_model10X)
summary(regression_model12X)
summary(regression_model13X)
summary(regression_model14X)
summary(regression_model15X)
