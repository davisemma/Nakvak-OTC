rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library('ggthemes')

mean_temp <- read_csv("Air Temperature/air_temp_test.csv")

plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8,),
        plot.title = element_text(size = 8, vjust = 2, margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "panel",
        strip.text.x = element_text(size = 8, face = 'bold', hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),
        axis.line = element_line(colour = "#403F3F", size = 0.3),
        panel.border  = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)

#Mean july-august air temperature
jul_aug <- filter(mean_temp, Month == 7 | Month == 8) %>%
  group_by(Year) %>%
  summarise(sum_mean = mean(Tmean_C))

temp_plot <- ggplot(jul_aug, aes(x = Year, y = sum_mean, ))+
  geom_smooth(size = 0.4, color = '#B765A5', alpha = 0.2)+
  geom_line(size = 0.4, color = "#E59AD2")+
  ylab("Ave. air temp. (°C) (July-August)")+
  plot_theme+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))

temp_plot

#T greater than ...
warm_season <- filter(mean_temp, Month >=6 & Month <= 9)%>%
  mutate(t_1 = if_else(Tmean_C > 1, 1, 0),
         t_5 = if_else(Tmean_C > 5, 1, 0),
         t_10 = if_else(Tmean_C > 10, 1, 0)) %>%
  group_by(Year) %>%
  summarise(sum_1 = sum(t_1),
            sum_5 = sum(t_5),
            sum_10 = sum(t_10))%>%
  pivot_longer(., cols = sum_1:sum_10, names_to = "Thres_variable", values_to = "Total" )

days_plot <- ggplot(warm_season, aes(x = Year, y = Total, color = Thres_variable))+
  geom_smooth(method = 'loess', size = 0.4, alpha = 0.2)+
  geom_line(size = 0.4)+
  scale_color_manual(values = c("#C6C6C6", "#403F3F","#908E8E"))+
                     #labels=c("> 1°C", "> 5°C","> 10°C"))+
  ylab('Number of days (June-September)')+
  ylim(0, 140)+
  plot_theme+
  theme(legend.position = 'none')+
  geom_text(x=1984, y=40, label="    > 10°C", size = 2.5)+
  geom_text(x=1984, y=93, label="    > 5°C", size = 2.5)+
  geom_text(x=1984, y=130, label="    > 1°C", size = 2.5)+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))

days_plot

#Thawing/freezing DD ratio 
thaw_freeze <- filter(mean_temp, Month >=4 & Month <= 9)%>%
  mutate(DD = if_else(Tmean_C > 0, 'thaw', 'freeze')) %>%
  group_by(Year) %>%
  summarise(Freeze_dd = (sum(Tmean_C[DD=="freeze"]))*-1,
            Thaw_dd = sum(Tmean_C[DD=="thaw"]),
            Ratio = Thaw_dd/Freeze_dd)

ratio_plot <- ggplot(thaw_freeze, aes(x = Year, y = Ratio))+
  geom_smooth(size = 0.4, color = '#46ABD2', alpha = 0.2)+
  geom_line(size = 0.4, color = "#81CBE9")+
  ylab("Thawing:freezing degree days (April-September)")+
  plot_theme+
  theme(plot.margin = unit(c(0.2, 0, 0.2, 0), "cm"))
ratio_plot

cowplot::plot_grid(temp_plot, ratio_plot, days_plot,
                   labels = c("A", "B", "C"),
                   label_size = 10,
                   ncol = 3)







