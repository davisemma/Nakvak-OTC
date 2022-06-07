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
         plot = substr(plot, 24, 35),
         subsite = substr(plot, 10,12),
         treatment = substr(plot, 6,8))

#Read in AIR TEMPS
air <- read_csv("OTC Temp Effect/Air/NAKVAK_DAILY_SAT_INTERP.csv")
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
  group_by(plot, year, subsite, treatment) %>%
  summarise(n_obs = n(),
            mean_diff_sa = mean(diff_sa)) %>% #On average, how much warmer is ground than air in the summer?
  mutate(plot_num = substr(plot, 3, 4)) %>%
  filter(., n_obs > 90) #Select plots with full obs only 

ggplot(ground_air_summary, aes(x = year, y = mean_diff_sa, color = treatment))+
  geom_hline(yintercept = 0, color = 'dark grey')+
  geom_point(alpha = 0.5, shape = 20, size = 4)+
  geom_smooth(alpha = 0.2)+
  theme_bw()+
  scale_color_manual(values = c("hot pink", "blue"))+
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020))+
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
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020))+
  ylab('OTC(offset) - CTL(offset)')+
  ggtitle('How much warmer are OTC ground temps (than the air) than CTL?')

#1. The difference in OTC and CTL is relatively constant over time, with offset of ~0.75
#2. At beginning, OTCs have cooler soil than air in wet plots relative to controls, over time this changes to warmer soil than air
#3. In general, soil temps are lower in wet plots than dry plots
#4. Soil temps often cooler than air temps in wet plots, usually warmer than air temps in dry plots






  