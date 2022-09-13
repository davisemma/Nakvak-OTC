rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggthemes)
library('roll')

setwd("Data")

#Read in GROUND TEMPS
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

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
         GST_thzc = if_else(abs(t_ground) <= 0.50, 1, 0))

test <- ground_hourly %>%
  mutate(rolling_prior = rollapplyr(t_ground, 24, sd, fill = NA),
         rolling_after = rollapplyr(t_ground, 24, sd, align =c("left"), fill = NA))

test2 <- test %>%
  mutate(SD_prior = if_else(rolling_prior <= 0.25, 1, 0),
         SD_after = if_else(rolling_after <= 0.25, 1, 0),
         cond_prior = GST_thzc + SD_prior,
         cond_after = GST_thzc + SD_after)

test3 <- test2 %>%
  mutate(zero_curtain = if_else(cond_prior == 2 | cond_after == 2, 1, 0))
derp <- test3
names(derp)

z <- derp %>%
  group_by(plot, year, subsite, treatment, short_date) %>%
  summarise(curtain_sum = sum(zero_curtain)) %>%
  mutate(zero_bin = if_else(curtain_sum > 6, 1, 0))

e <- z %>%
  group_by(subsite, treatment, short_date) %>%
  summarise(test = sum(zero_bin),
            obs = n(),
            prop = test/obs)


ggplot(e, aes(yday(short_date), y = prop, color = subsite))+
  geom_point()
  




early <- derp %>%
  filter(., zero_curtain == 1) %>%
  filter(., month < 8) %>%
  group_by(plot, year) %>%
  slice_min(date) %>%
  mutate(start_date = short_date)
  

late <- derp %>%
  filter(., zero_curtain == 1) %>%
  filter(., month < 8) %>%
  group_by(plot, year) %>%
  slice_max(date) %>%
  mutate(end_date = short_date)

y <- inner_join(late[(c("plot", "year", "subsite", "treatment", "end_date"))], early[(c("plot", "year", "subsite", "treatment", "start_date"))])

yday(y$end_date)

ggplot(y, aes(x = start_date, y = plot, color = subsite))+
  geom_linerange(aes(xmin=start_date,xmax=end_date),linetype=1)

