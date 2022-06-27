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
ground_month_select <- filter(ground, month(date) == 6 | month(date) == 7 | month(date) == 8)
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
  filter(., n_obs > 85) #Select plots with > 90% data avail

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
  mutate(diff_otc_ctl = OTC-CTL) %>%
  filter(., n_obs > 90)

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

ground_hourly <- tbl_with_sources_hourly
names(ground_hourly) <- c("date", "t_ground", "plot")
ground_hourly <- ground_hourly %>%
  mutate(year = as.integer(substr(date, 1, 4)),
         plot = substr(plot, 31, 42),
         subsite = substr(plot, 10,12),
         treatment = substr(plot, 6,8))

test <- ground_hourly %>%
  mutate(short_date = as_date(date)) %>%
  group_by(plot, short_date) %>%
  #complete(year, subsite, treatment) %>%
  summarise(n_obs = n(),
            min_t_ground = min(t_ground),
            max_t_ground = max(t_ground),
            daily_t_var = max_t_ground-min_t_ground) %>%
  filter(., n_obs == 24) %>%
  mutate(year = substr(short_date, 1,4),
         year_plot = paste(year, plot, sep = "_")) %>%
  mutate(subsite = substr(year_plot, 15,17),
       treatment = substr(year_plot, 11,13),
       year = as.integer(substr(year_plot, 1,4)),
       year_scale = scale(year),
       plot = as.factor(substr(year_plot, 6,9)))

ggplot(test, aes(x = short_date, y = daily_t_var, color = subsite))+
  geom_line()+
  facet_wrap(~as.factor(year), scales ="free_x")

test_lt1 <- test %>%
  filter(., month(short_date) <= 6 | month(short_date) >= 9) %>%
  mutate(day_lt1 = if_else(daily_t_var <= 1, 1, 0))

ggplot(test_lt1, aes(x = short_date, y = day_lt1))+
  geom_point()+
  facet_wrap(~year, scales = "free_x")


test2 <- test %>%
  #filter(., month(short_date) <= 6 | month(short_date) >= 9) %>% #Dont need to filter bc var > 1 extremely infrequent
  mutate(days_lt1 = if_else(daily_t_var <= 1, 1, 0)) %>%
  group_by(year_plot) %>%
  summarise(n_obs = sum(!is.na(days_lt1)),
            freq_lt1 = sum(days_lt1, na.rm = TRUE)) %>%
  filter(., n_obs >= 360) %>%
  #drop_na() %>%
  mutate(subsite = substr(year_plot, 15,17),
         treatment = substr(year_plot, 11,13),
         year = as.integer(substr(year_plot, 1,4)),
         year_scale = scale(year),
         plot = as.factor(substr(year_plot, 6,9)))


ggplot(test2, aes(x = as.factor(year), y = freq_lt1, fill = treatment, alpha = 0.5))+
  geom_boxplot()+
  theme_pubr()+
  facet_wrap(~subsite)+
  scale_fill_manual(values = c("coral", "cornflower blue"))+
  ylab("#Days Tvar < 1°C")+
  xlab("Year")


#library(glmmTMB)
dry_dat<-filter(test2, subsite == 'Dry' & year > 2011)
dry_sno_mod <- glmmTMB(freq_lt1 ~ treatment + year_scale + (1|plot) + (1|year_scale), 
        family = nbinom1,
        data = dry_dat)
summary(dry_sno_mod)
check_model(dry_sno_mod) 

wet_dat<-filter(test2, subsite == 'Dry' & year > 2011)
wet_sno_mod <- glmmTMB(freq_lt1 ~ treatment + year_scale + (1|plot) + (1|year_scale), 
               family = nbinom1,
               data = wet_dat)
summary(wet_sno_mod)
check_model(wet_sno_mod) 


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
  