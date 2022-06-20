rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)

setwd("Data")

mean_temp <- read_csv("Air Temperature/ERA5_mean_2m_temperature_monthly.csv")%>%
  mutate(full_date = dmy(date),
         month = factor(substr(date, 4,6), levels = 
                          c("Jan", "Feb", "Mar",
                        "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep",
                        "Oct", "Nov", "Dec")),
         year = as.numeric(substr(full_date, 1,4)),
         plot_order = month(full_date))


min_temp <- read_csv("Air Temperature/ERA5_min_2m_temperature_monthly.csv")%>%
  mutate(full_date = dmy(date),
         month = factor(substr(date, 4,6), levels = 
                          c("Jan", "Feb", "Mar",
                            "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec")),
         year = as.numeric(substr(full_date, 1,4)),
         plot_order = month(full_date))

max_temp <- read_csv("Air Temperature/ERA5_max_2m_temperature_monthly.csv")%>%
  mutate(full_date = dmy(date),
         month = factor(substr(date, 4,6), levels = 
                          c("Jan", "Feb", "Mar",
                            "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec")),
         year = as.numeric(substr(full_date, 1,4)),
         plot_order = month(full_date))


ggplot(mean_temp, aes(x = year, y = temp_c, color = month))+
  geom_rect(
    fill = "yellow", color = NA, alpha = 0.01, ,
    xmin = 2008,
    xmax = 2021,
    ymin = -Inf,
    ymax = Inf)+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~month, scales = "free_y")

ggplot(min_temp, aes(x = year, y = min_temp_c, color = month))+
  geom_rect(
    fill = "yellow", color = NA, alpha = 0.01, ,
    xmin = 2008,
    xmax = 2021,
    ymin = -Inf,
    ymax = Inf)+
  geom_line()+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~month, scales = "free_y")


ggplot(max_temp, aes(x = year, y = max_temp_c, color = month))+
  geom_line()+
  geom_smooth()+
  geom_vline(xintercept = c(2008, 2010,2015, 2021), color = 'red')+
  theme_bw()+
  facet_wrap(~month, scales = "free_y")




ggplot(temp, aes(x = year, y = temp_c, color = month))+
  geom_line()+
  geom_smooth(method = 'lm')+
  theme_bw()+
  facet_wrap(~month, scales = "free_y")
