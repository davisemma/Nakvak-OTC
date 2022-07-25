rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library('ggthemes')

mean_temp <- read_csv("Air Temperature/NAK_monthly_mean_temp.csv") %>%
  mutate(month_long = month(month, label = TRUE, abbr = FALSE))

plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -3, b = -10, l = -22, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = 'bold'),
        plot.title = element_text(size = 10, vjust = 2, face = 'bold', margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 8, face = 'bold'),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8, face = 'bold'),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(colour = 'black', size = 0),
        strip.text.x = element_text(size = 8),
        panel.border = element_rect(size = .4),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"))

ggplot(mean_temp, aes(x = year, y = t_ave))+
  ggtitle("Average monthly temperatures from 1981 to 2021")+
  geom_rect(
    fill = "#E5EDF8", color = NA,
    xmin = 2008,
    xmax = 2021,
    ymin = -Inf,
    ymax = Inf)+
  geom_smooth(size = 0.5, fill = '#FFBBFF', color = '#B765A5')+
  geom_line(size = 0.2)+
  scale_x_continuous(breaks = c(1980, 2000, 2020))+
  facet_wrap(~month_long, scales = "free_y")+
  ylab('Temperature (°C)')+
  xlab('Year')+
  plot_theme

#4 x 6 export

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
