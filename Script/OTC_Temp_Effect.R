rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggthemes)

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

#Plotting 
plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 10, vjust = 2, face = 'bold', margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(colour = 'black', size = 0),
        strip.text.x = element_text(size = 8, hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        panel.border = element_rect(size = .3),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),)

ground_air_summary <- ground_air_merge %>%
  filter(!is.na(diff_sa)) %>%
  group_by(plot, year, subsite, treatment) %>%
  summarise(n_obs = n(),
            mean_diff_sa = mean(diff_sa)) %>% #On average, how much warmer is ground than air in the summer?
  mutate(plot_num = substr(plot, 3, 4)) %>%
  filter(., n_obs > 55) #Select plots with > 90% data avail


facet_labels <- as_labeller(c('Dry' = 'Dry plots', 'Wet' = 'Wet plots'))

ggplot(ground_air_summary, aes(x = year, y = mean_diff_sa, color = treatment))+
  geom_hline(yintercept = 0, color = 'dark grey')+
  geom_point(alpha = 0.5, shape = 20, size = 4)+
  geom_smooth(alpha = 0.2)+
  scale_color_manual(values = c("hot pink", "blue"))+
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020, 2022))+
  facet_wrap(~subsite, labeller = facet_labels)+
  ylab('Seasonal average of (Tground - Tair)')+
  ggtitle('How much warmer (+) or cooler (-) is ground than air in summer?')+
  plot_theme
  
ga_wide <- pivot_wider(ground_air_summary[2:7], names_from = treatment, values_from = mean_diff_sa) %>%
  mutate(diff_otc_ctl = OTC-CTL)

#Calculate direct OTC effect --- 
otc_effect <- ground_month_select %>%
  mutate(plot = substr(plot, 1,4))%>%
  pivot_wider(., names_from = treatment, values_from = t_ground) %>%
  mutate(OTC_effect = OTC - CTL) %>%
  filter(!is.na(OTC_effect)) %>%
  group_by(plot, year, subsite) %>%
  summarise(ave_OTC_effect = mean(OTC_effect),
            n_obs = n()) %>%
  filter(., n_obs > 55)
  
#Calculating average OTC effect ---
otc_effect_summary <- otc_effect %>%
  filter(., year < 2019) %>%
  group_by(subsite) %>%
  summarise(mean = mean(ave_OTC_effect),
            SD = sd(ave_OTC_effect))
  
#Difference in ground temperatures between OTC and CTL - 
ggplot(otc_effect, aes(x = as.integer(year), y = ave_OTC_effect, color = subsite, fill = subsite))+
  geom_hline(yintercept = 0, color = 'dark grey')+
  geom_smooth(alpha = 0.5, size = 0.75, show.legend = FALSE)+
  geom_point(alpha = 0.9, shape = 21, size = 2, color = 'black')+
  theme_bw()+
  scale_fill_manual(values = c("#F3E086", "#BED4F1"), name = 'Moisture class')+
  scale_color_manual(values = c("#F3E086", "#BED4F1"), name = 'Moisture class')+
  ylab('Average effect (°C)')+
  xlab('')+
  ggtitle('Effect of OTC on summer GST by soil moisutre class')+
  plot_theme+
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018),
                     labels = c(2012, '', 2014, '', 2016, '', 2018),
                     limits = c(2012, 2018))

#export 4 x 4 inches


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
  ggtitle("Number of snow days per year")+
  geom_boxplot(size = .3, outlier.size = 0.75, position = position_dodge2(preserve = "single"))+
  scale_x_discrete(breaks=seq(2010, 2020, 2))+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = 'Treatment')+
  labs(y = '# Snow days',
       x = 'Year')+
  facet_wrap(~subsite, labeller = facet_labels)+
  plot_theme

snow_day_mod <- glmmTMB(n_snow_days ~ treatment + scale(year) + (1|plot) + (1|year), 
                        family = 'gaussian', data = filter(snow_day_complete, subsite == 'Dry'))
summary(snow_day_mod)
check_model(snow_day_mod)
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


snow_duration <- merge(dplyr::select(snow_offset, c("plot", "year", "year_plot", "subsite", "treatment", "doy_offset")),
           dplyr::select(snow_onset, c("plot", "year", "year_plot", "subsite", "treatment", "doy_onset")), all.x = TRUE) %>%
  mutate(snow_free = abs(doy_offset - doy_onset))

###Still possible that big gaps could lead to skewed detection of onset and offset

ggplot(snow_duration, aes(x = as.factor(year), y = snow_free , fill = treatment))+
  ggtitle("Duration of snow free period")+
  geom_boxplot(size = .2, outlier.size = 0.5, position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = 'Treatment')+
  labs(y = '# Days',
       x = '')+
  facet_wrap(~subsite, labeller = facet_labels)+
  scale_x_discrete(breaks=seq(2010, 2020, 2))+
  plot_theme
  
snow_duration_mod <- glmmTMB(snow_free ~ treatment*scale(year) + (1|plot) + (1|year), family = 'gaussian', data = filter(snow_duration, subsite == 'Dry'))
summary(snow_duration_mod)

check_model(snow_duration_mod)

mod <- glmmTMB(snow_free ~ treatment*scale(year) + (1|plot) + (1|year), family = 'gaussian', data = filter(snow_duration, subsite == 'Wet'))
summary(mod)

#OTC may have some influence on reducing the number of snow covered days at dry sites
#Since it's CTL that has more snow days than OTC, doesn't seem to be trapping but rather longer 
#longer snow-free season caused by warmer temps 

ggplot(snow_duration)+
  geom_point(aes(x = year, y = doy_onset, color = treatment))+
  geom_point(aes(x = year, y = doy_offset, color = treatment))+
  facet_wrap(~subsite)
  
x <- snow_duration %>%
  group_by(treatment, subsite) %>%
  summarise(mean_onset = mean(doy_onset, na.rm = TRUE),
            mean_offset = mean(doy_offset),
            diff = mean_onset - mean_offset)
  






  