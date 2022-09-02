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

#Plotting 
plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -10, l = -6, unit = "pt"),
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
        panel.border = element_rect(size = .4),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),)


dry <- ggplot(filter(ground, subsite == 'Dry'), aes(x = date, y = t_ground, group = plot))+
  geom_line()+
  facet_wrap(~plot, ncol = 3)+
  plot_theme+
  ylab('Average daily ground surface temperature (°C)')+
  xlab('Date')
dry

wet <- ggplot(filter(ground, subsite == 'Wet'), aes(x = date, y = t_ground, group = plot))+
  geom_line()+
  facet_wrap(~plot, ncol = 3)+
  plot_theme+
  ylab('Average daily ground surface temperature (°C)')+
  xlab('Date')
wet

unique(ground$plot)





