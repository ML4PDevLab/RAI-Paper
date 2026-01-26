dat = rai.atari::rai %>%
  filter(influencer == "Russia" & country %in% c("Georgia", "Moldova", "Ukraine")) %>%
  filter(date >= "2018-01-01")

ggplot(dat, aes(x = date, y =  rai_idx_di)) +
  geom_line(linewidth = 1, color = "black", linetype=1) +
  facet_wrap(~country, scales = "free_y", ncol = 1) +
  geom_vline( xintercept = as.numeric(as.Date("2023-11-01")), linetype="dashed", color = "#66a182" ) +
  theme_bw() +
  scale_x_date(breaks = seq(as.Date("2012-01-01"), as.Date(max(dat$date)), by = "6 months"), labels = scales::date_format("%Y-%b"),
               expand = c(.01,.01) ) +
  theme(plot.title = element_text(hjust = 0, size = 18),  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 18)) +
  labs(y = "Domestic Interferrence (articles per 10k published)")