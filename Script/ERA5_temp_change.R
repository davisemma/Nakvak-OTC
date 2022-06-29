rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library('ggthemes')

setwd("Data")

mean_temp <- read_csv("Air Temperature/NAK_monthly_mean_temp.csv") %>%
  mutate(month_long = month(month, label = TRUE, abbr = FALSE))

ggplot(mean_temp, aes(x = year, y = t_ave))+
  geom_rect(
    fill = "lightblue", color = NA, alpha = 0.01,
    xmin = 2008,
    xmax = 2021,
    ymin = -Inf,
    ymax = Inf)+
  geom_smooth(size = 0.3, alpha = 0.2, fill = 'purple', color = 'red')+
  geom_line(size = 0.3)+
  theme_few()+
  facet_wrap(~month_long, scales = "free_y")+
  ylab('Mean temperature (°C)')+
  xlab('Year')+
  ggtitle("Mean monthly temperatures 1981 - 2021")

###
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
