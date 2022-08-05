x <- getSunlightTimes(date = ground$date, lat = 58.64, lon = 63.35, tz ="-3 UTC")

#Read in hourly data 
tbl_with_sources_hourly <-
  list.files(path = "OTC Temp Effect/Ground Hourly",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

ground_hourly <- tbl_with_sources_hourly %>%
  set_names(., c("date", "t_ground", "plot")) %>% 
  mutate(year = as.integer(substr(date, 1, 4)),
         plot = substr(plot, 31, 42),
         subsite = substr(plot, 10,12),
         treatment = substr(plot, 6,8),
         short_date = as_date(date),
         month = month(date),
         hour = hour(date),
         day_night = if_else(hour < 4 | hour > 20, 'night', 'day'))

x <- ground_hourly %>%
  group_by(plot, year, subsite, treatment, short_date, month, day_night) %>%
  summarise(ave_t = mean(t_ground),
            n_obs = sum(!is.na(t_ground))) %>%
  filter(., month == 7 | month == 8) %>%
  mutate(filter_cond_day = if_else(day_night == 'day' & n_obs == 17, 'keep', 'go'),
         filter_cond_night = if_else(day_night == 'night' & n_obs == 7, 'keep', 'go')) %>%
  filter(filter_cond_day == 'keep' | filter_cond_night == 'keep')

a <- x %>%
  group_by(plot, year, subsite, treatment, day_night) %>%
  summarise(ave_t = mean(ave_t),
            n_obs = n()) %>%
  filter(., n_obs >= 50) %>%
  mutate(plot = substr(plot, 1, 4))

b <- pivot_wider(a, names_from = treatment, values_from = ave_t) %>%
  mutate(otc_effect = OTC - CTL)

ggplot(b, aes(x = year, y = otc_effect, color = subsite))+
  geom_point()+
  geom_smooth()+
  xlim(2012, 2019)+
  facet_wrap(~day_night)+
  plot_theme

ggplot(b, aes(x = year, y = otc_effect, color = day_night))+
  geom_point()+
  geom_smooth()+
  xlim(2012, 2019)+
  facet_wrap(~subsite)+
  plot_theme

###
z <- x %>%
  mutate(plot = substr(plot, 1, 4)) %>%
  pivot_wider(., names_from = treatment, values_from = ave_t ) %>%
  mutate(otc_effect = OTC - CTL,
         doy = yday(short_date))

ggplot(z, aes(x = doy, y = otc_effect, color = day_night))+
  geom_smooth()+
  facet_wrap(subsite~year)








