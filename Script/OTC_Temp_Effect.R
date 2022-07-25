rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)

setwd("Data")

#Read in GROUND TEMPS
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(path = "OTC Temp Effect/Ground Daily",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))

ground <- tbl_with_sources
names(ground) <- c("date", "t_ground", "plot")
ground <- ground %>%
  mutate(year = as.integer(substr(date, 1, 4)),
         plot = substr(plot, 30, 41),
         subsite = substr(plot, 10,12),
         treatment = substr(plot, 6,8))

#Read in AIR TEMPS
air <- read_csv("OTC Temp Effect/Air/ERA5_daily_air_2010_2021.csv")
names(air) <- c("date", "t_air")
air <- air %>%
  mutate(year = as.integer(substr(date, 1, 4)))

#Subsetting data for analysis of summer OTC effect 
ground_month_select <- filter(ground, month(date) == 7 | month(date) == 8)
ground_air_merge <- merge(ground_month_select, air, all.x = TRUE)%>%
  mutate(diff_sa = t_ground - t_air) #How much warmer is ground than air on a given day? (if+)

ggplot(ground_air_merge, aes(x = year, y = diff_sa, color = treatment))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~subsite)+
  ylim(-2,2)

ground_air_summary <- ground_air_merge %>%
  filter(!is.na(diff_sa)) %>%
  group_by(plot, year, subsite, treatment) %>%
  summarise(n_obs = n(),
            mean_diff_sa = mean(diff_sa)) %>% #On average, how much warmer is ground than air in the summer?
  mutate(plot_num = substr(plot, 3, 4)) %>%
  filter(., n_obs > 55) #Select plots with > 90% data avail

ggplot(ground_air_summary, aes(x = year, y = mean_diff_sa, color = treatment))+
  geom_hline(yintercept = 0, color = 'dark grey')+
  geom_point(alpha = 0.5, shape = 20, size = 4)+
  geom_smooth(alpha = 0.2)+
  theme_bw()+
  scale_color_manual(values = c("hot pink", "blue"))+
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020, 2022))+
  facet_wrap(~subsite)+
  ylab('Seasonal average of (Tground - Tair)')+
  ggtitle('How much warmer (+) or cooler (-) is ground than air in summer?')
  
ga_wide <- pivot_wider(ground_air_summary[2:7], names_from = treatment, values_from = mean_diff_sa) %>%
  mutate(diff_otc_ctl = OTC-CTL)

#Difference in ground temperatures between OTC and CTL - 
ggplot(ga_wide, aes(x = as.integer(year), y = diff_otc_ctl, color = subsite))+
  geom_hline(yintercept = 0, color = 'dark grey')+
  geom_point(alpha = 0.5, shape = 20, size = 4)+
  geom_smooth(alpha = 0.2)+
  theme_bw()+
  scale_color_manual(values = c("coral", "cornflower blue"))+
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020, 2022))+
  ylab('OTC(offset) - CTL(offset)')+
  xlab('Year')+
  ggtitle('How much warmer are OTC ground temps (than the air) than CTL?')

#1. The difference in OTC and CTL is relatively constant over time, with offset of ~0.75
#2. At beginning, OTCs have cooler soil than air in wet plots relative to controls, over time this changes to warmer soil than air
#3. In general, soil temps are lower in wet plots than dry plots
#4. Soil temps often cooler than air temps in wet plots, usually warmer than air temps in dry plots

#Calculating SNOW DAY from ground temperature ----
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
         short_date = as_date(date))

daily_t_var <- ground_hourly %>%
  group_by(plot, short_date) %>%
  summarise(n_obs = sum(!is.na(t_ground)), #number of actual temperature observations per day
            min_t_ground = min(t_ground),
            max_t_ground = max(t_ground),
            daily_t_var = max_t_ground-min_t_ground) %>%
  ungroup()%>%
  mutate(daily_t_var = if_else(as.numeric(n_obs) != 24, NA_real_, daily_t_var), #fill days without 24 hourly observations with NA
         year = substr(short_date, 1,4),
         year_plot = paste(year, plot, sep = "_"),
         subsite = substr(year_plot, 15,17),
         treatment = substr(year_plot, 11,13),
         year = as.integer(substr(year_plot, 1,4)),
         year_scale = scale(year),
         plot = as.factor(substr(year_plot, 6,9)))

t_var <- daily_t_var %>%
  filter(., year > 2011) %>% #Bad data coverage for 2010/2011 so remove 
  mutate(day_var_lt1 = if_else(daily_t_var <= 1, 1, 0), #Boolean t_var <= 1
         day_max_lt3 = if_else(max_t_ground <=3, 1, 0), #Boolean t_max <= 3
         snow_day = if_else(day_var_lt1+day_max_lt3 == 2, 1, 0))%>% #Value == 2 means t_var and t_max meet criteria, classify as '1'
  group_by(plot, treatment) %>%
  mutate(rolling_snow_day = zoo::rollsumr(snow_day, k = 3, fill = NA, align = 'left')) #value = 3 in rolling_snow_day output means three next days with t_var <= 1 AND max_t <= 3

#n snow days
data_completeness <- t_var %>%
  group_by(plot, year, subsite, treatment, year_plot) %>%
  summarize(n_obs = sum(!is.na(daily_t_var))) %>%
  filter(., n_obs > 350)
  
snow_day_complete <- t_var %>%
  filter(., snow_day == 1) %>%
  group_by(plot, year, subsite, treatment, year_plot) %>%
  summarise(n_snow_days = sum(snow_day)) %>%
  merge(data_completeness, ., all.x = TRUE)

ggplot(snow_day_complete, aes(x = as.factor(year), y = n_snow_days , fill = treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75, )+
  theme_pubr(base_size = 10, legend = 'bottom')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("plum1", 'seagreen3'))+
  labs(y = '# Snow days',
       x = 'Year')+
  facet_wrap(~subsite)  

snow_day_mod <- glmmTMB(n_snow_days ~ treatment + scale(year) + (1|plot) + (1|year), 
                        family = 'gaussian', data = filter(snow_day_complete, subsite == 'Dry'))
summary(snow_day_mod)
#Fewer snow days in OTC

#Snow onset and offset ---
snow_onset <- t_var %>%
  filter(., month(short_date) >= 9 & month(short_date) <= 11) %>%
  filter(., rolling_snow_day == 3) %>%
  group_by(plot, year, treatment) %>%
  mutate(onset_date = min(short_date)) %>%
  group_by(plot, year, treatment) %>%
  filter(short_date==onset_date) %>%
  mutate(doy_onset = yday(onset_date))
  
snow_offset <- t_var %>%
  group_by(year_plot) %>%
  filter(., month(short_date) <= 7) %>%
  filter(., rolling_snow_day == 3) %>%
  mutate(offset_date = max(short_date)) %>%
  group_by(plot, year, treatment) %>%
  filter(short_date==offset_date) %>%
  mutate(doy_offset = yday(offset_date))


snow_duration <- merge(select(snow_offset, c("plot", "year", "year_plot", "subsite", "treatment", "doy_offset")),
           select(snow_onset, c("plot", "year", "year_plot", "subsite", "treatment", "doy_onset")), all.x = TRUE) %>%
  mutate(snow_free = abs(doy_offset - doy_onset))

###Still possible that big gaps could lead to skewed detection of onset and offset

ggplot(snow_duration, aes(x = as.factor(year), y = snow_free , fill = treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75, )+
  theme_pubr(base_size = 10, legend = 'bottom')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("plum1", 'seagreen3'))+
  labs(y = '# Snow free days',
       x = 'Year')+
  facet_wrap(~subsite)
  
snow_duration_mod <- glmmTMB(snow_free ~ treatment*scale(year) + (1|plot) + (1|year), family = 'gaussian', data = filter(snow_duration, subsite == 'Dry'))
summary(snow_duration_mod)

check_model(snow_duration_mod)

mod <- glmmTMB(snow_free ~ treatment*scale(year) + (1|plot) + (1|year), family = 'gaussian', data = filter(snow_duration, subsite == 'Wet'))
summary(mod)

#OTC may have some influence on reducing the number of snow covered days at dry sites
#Since it's CTL that has more snow days than OTC, doesn't seem to be trapping but rather longer 
#longer snow-free season caused by warmer temps 

#Looking at temp diffs between pairs of OTC and CTL
ground_otc_ctl <- ground %>%
  mutate(plot_num = substr(plot, 3, 4)) %>%
  select(-plot)

ggplot(ground_otc_ctl, aes(x = date, y = t_ground, color = treatment))+
  geom_line()+
  facet_wrap(~subsite)


x <- pivot_wider(ground_otc_ctl, names_from = treatment, values_from = t_ground) %>%
  mutate(diff_otc_ctl = OTC-CTL)

ggplot(x, aes(x = date, y = diff_otc_ctl, color = subsite))+
  geom_line(alpha = 0.7)

x2 <- x %>%
  group_by(date, subsite) %>%
  summarise(mean_diff = mean(diff_otc_ctl, na.rm = TRUE)) %>%
  mutate(year = substr(date, 1,4))

ggplot(x2, aes(x = date, y = mean_diff, color = subsite))+
  geom_line()+
  facet_wrap(~as.factor(year), scales ="free_x")
  