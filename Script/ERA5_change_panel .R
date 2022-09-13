rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library('ggthemes')

mean_temp <- read_csv("Air Temperature/NAK_air_temp_vars.csv")
ppt <- read_csv("Air Temperature/NAK_1981_2022_all_vars.csv")

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
  geom_smooth(size = 0.4, color = '#E18D6E', alpha = 0.2)+
  geom_line(size = 0.4, color = "#965635")+
  ylab("Jul-Aug temp. (°C)")+
  plot_theme+
  theme(plot.margin = unit(c(0.25,0.2,0.2,0.3), "cm"))

temp_plot

#T greater than ...
warm_season <- filter(mean_temp, Month == 7 | Month == 8)%>%
  mutate(t_5 = if_else(Tmean_C > 5, 1, 0),
         t_10 = if_else(Tmean_C > 10, 1, 0),
         t_15 = if_else(Tmean_C > 15, 1, 0)) %>%
  group_by(Year) %>%
  summarise(sum_5 = sum(t_5),
            sum_10 = sum(t_10),
            sum_15 = sum(t_15))%>%
  pivot_longer(., cols = sum_5:sum_15, names_to = "Thres_variable", values_to = "Total" )

days_plot <- ggplot(warm_season, aes(x = Year, y = Total, color = Thres_variable))+
  geom_smooth(method = 'loess', size = 0.4, alpha = 0.2)+
  geom_line(size = 0.4)+
  scale_color_manual(values = c("#908E8E", "#403F3F","#C6C6C6"))+
  ylab('# Days (Jul-Aug)')+
  plot_theme+
  theme(legend.position = 'none')+
  geom_text(x=1984, y=62, label="    > 5°C", size = 2.5)+
  geom_text(x=1984, y=28, label="    > 10°C", size = 2.5)+
  geom_text(x=1984, y=8, label="    > 15°C", size = 2.5)+
  theme(plot.margin = unit(c(0.25,0.2,0.2,0.3), "cm"))

days_plot

#Thawing/freezing DD ratio 
thaw_freeze <- filter(mean_temp, Month >=4 & Month <= 9)%>%
  mutate(DD = if_else(Tmean_C > 0, 'thaw', 'freeze')) %>%
  group_by(Year) %>%
  summarise(Freeze_dd = (sum(Tmean_C[DD=="freeze"]))*-1,
            Thaw_dd = sum(Tmean_C[DD=="thaw"]),
            Ratio = Thaw_dd/Freeze_dd)

ratio_plot <- ggplot(thaw_freeze, aes(x = Year, y = Ratio))+
  geom_smooth(size = 0.4, color = '#ABAA35', alpha = 0.2)+
  geom_line(size = 0.4, color = "#106003")+
  ylab("Thaw:freeze (Apr-Sep)")+
  plot_theme+
  theme(plot.margin = unit(c(0.25,0.2,0.2,0.3), "cm"))
ratio_plot


#PRECIPITATION
ppt_jul_aug <- filter(ppt, Month == 7 | Month == 8) %>%
  group_by(Year) %>%
  summarise(ppt_sum = sum(Precipitation),
            var = c('jul-aug'))


ppt_plot <- ggplot(ppt_jul_aug, aes(x = Year, y = ppt_sum))+
  geom_smooth(size = 0.4, color = '#A8DCFC', alpha = 0.2)+
  geom_line(size = 0.4, color = "#82AAD9")+
  ylab("Jul-Aug precip. (mm)")+
  plot_theme+
  theme(plot.margin = unit(c(0.25,0.2,0.2,0.3), "cm"))

ppt_plot

cowplot::plot_grid(temp_plot, days_plot, ratio_plot, ppt_plot,
                   labels = c("A", "B", "C", "D"),
                   label_x = -.001,
                   label_y = 1.02,
                   label_size = 10,
                   ncol = 2)

cowplot::plot_grid(temp_plot, ppt_plot, days_plot, ratio_plot,
                   labels = c("A", "B", "C", "D"),
                   label_x = -.001,
                   label_y = 1.02,
                   label_size = 10,
                   ncol = 4)
cowplot::plot_grid(temp_plot, ppt_plot, days_plot, ratio_plot,
                   labels = c("A", "B", "C", "D"),
                   label_size = 10,
                   ncol = 1)





