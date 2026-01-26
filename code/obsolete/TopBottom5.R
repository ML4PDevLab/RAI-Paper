library(tradestatistics)
library(tidyverse)
library(dplyr)
library(estimatr)

TradeDataHS2 = readr::read_csv(here::here("data/TradeDataHS2.csv"))


names(TradeDataHS2)[names(TradeDataHS2) == 'imp_rank_hs2_5yr...10'] <- 'imp_rank_hs2_5yr'
names(TradeDataHS2)[names(TradeDataHS2) == 'imp_rank_hs2_5yr...11'] <- 'exp_rank_hs2_5yr'

TradeDataHS2 <- TradeDataHS2 %>%
  group_by(reporter_name) %>%
  mutate(
    tbal_HS2_5yr = exp_sum_hs2_5yr - imp_sum_hs2,
    import_factor = imp_sum_hs2/(exp_sum_hs2_5yr + imp_sum_hs2),
    export_factor = exp_sum_hs2_5yr/(exp_sum_hs2_5yr + imp_sum_hs2),
    trade_share = (exp_sum_hs2_5yr + imp_sum_hs2)/(sum(exp_sum_hs2_5yr)+sum(imp_sum_hs2)),
    impTop5 = case_when(imp_rank_hs2_5yr < 6 ~ 1,TRUE ~ 0),
    expTop5 = case_when(exp_rank_hs2_5yr < 6 ~ 1,TRUE ~ 0),
    impTop10 = case_when(imp_rank_hs2_5yr < 11 ~ 1,TRUE ~ 0),
    expTop10 = case_when(exp_rank_hs2_5yr < 11 ~ 1,TRUE ~ 0)
  )
  view(TradeDataHS2)
  
dfpr <- readr::read_csv(here::here("data/dfpr-1.csv"))
  
  
  #___
  
  TradeDataHS2 <- TradeDataHS2 %>%
    left_join(dfpr %>% select(country, mean_total_rai), 
              by = c("reporter_name" = "country"))%>%
    filter(!is.na(mean_total_rai))
  
  write_csv(TradeDataHS2, here::here("data/TradeDataHS2edited.csv"))
  
  top5Tallies <- readr::read_csv(here::here("data/Top5Tallies.csv"))
  
  view(top5Tallies)
  
  top5Tallies <- top5Tallies %>%
    mutate(
      Top5 = case_when(impTop5 == 1 | expTop5 == 1 ~ 1,TRUE ~ 0)
      )
  
  mean (top5Tallies$mean_total_rai)
  
  top5Tallies %>% 
    filter(Top5 == 1) %>% 
    summarise(T5Mean=mean(mean_total_rai, na.rm=TRUE))
    tally(top5Tallies$Top5 == 1)
  mean(top5Tallies$mean_total_rai)
  
  top5Tallies %>% 
    filter(Top5 == 0) %>% 
    summarise(T5Mean=mean(mean_total_rai, na.rm=TRUE)) %>%
  tally(top5Tallies$Top5 == 0)
  mean(top5Tallies$mean_total_rai)
  
  top5Tallies %>% 
    filter( impTop5 == 1) %>% 
    summarise(impT5Mean=mean(mean_total_rai, na.rm=TRUE)) %>%
    tally(top5Tallies$impTop5 == 1)
  mean(top5Tallies$mean_total_rai)
  
  top5Tallies %>% 
    filter( expTop5 == 1) %>% 
    summarise(expT5Mean=mean(mean_total_rai, na.rm=TRUE)) %>%
  tally(top5Tallies$expTop5 ==1)
  
  top5Tallies %>% 
    filter( impTop5 == 0) %>% 
    summarise(impBottom=mean(mean_total_rai, na.rm=TRUE)) %>%
  #tally(top5Tallies$impTop5 ==0)
  
  top5Tallies %>% 
    filter( expTop5 == 0) %>% 
    summarise(expBottom=mean(mean_total_rai, na.rm=TRUE)) %>%
  tally(top5Tallies$expTop5 ==0)