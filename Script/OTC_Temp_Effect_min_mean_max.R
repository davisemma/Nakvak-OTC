rm(list=ls())
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggthemes)

#Read in hourly data 
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources_hourly <-
  list.files(path = "OTC Temp Effect/Ground Hourly",
             pattern = "*.csv", 
             full.names = T)%>% 
  map_df(~read_plus(.))

#Calculate vals

ground_hourly <- tbl_with_sources_hourly %>%
  set_names(., c("date", "t_ground", "plot")) %>% 
  mutate(year = as.integer(substr(date, 1, 4)),
         plot = substr(plot, 31, 42),
         subsite = substr(plot, 10,12),
         treatment = substr(plot, 6,8),
         short_date = as_date(date))

daily_t <- ground_hourly %>%
  group_by(plot, short_date) %>%
  summarise(n_obs = sum(!is.na(t_ground)), #number of actual temperature observations per day
            min_t_ground = min(t_ground),
            max_t_ground = max(t_ground),
            mean_t_ground = mean(t_ground)) %>%
  ungroup()%>%
  mutate(mean_t_ground = if_else(as.numeric(n_obs) != 24, NA_real_, mean_t_ground),
         min_t_ground = if_else(as.numeric(n_obs) != 24, NA_real_, min_t_ground),
         max_t_ground = if_else(as.numeric(n_obs) != 24, NA_real_, max_t_ground), #fill days without 24 hourly observations with NA
         year = as.numeric(substr(short_date, 1,4)),
         subsite = substr(plot, 10,12),
         treatment = substr(plot, 6,8),
         plot = as.factor(substr(plot, 1,4))) %>%
  filter(month(short_date) == 7 | month(short_date) == 8)


effect_wide <-   pivot_wider(daily_t, names_from = treatment, values_from = min_t_ground:mean_t_ground) %>%
  mutate(min_effect = min_t_ground_OTC - min_t_ground_CTL,
         max_effect = max_t_ground_OTC - max_t_ground_CTL,
         mean_effect = mean_t_ground_OTC - mean_t_ground_CTL) %>%
  filter(!is.na(mean_effect)) %>%
  group_by(plot, year, subsite) %>%
  summarise(mean_OTC_effect = mean(mean_effect),
            min_OTC_effect = mean(min_effect),
            max_OTC_effect = mean(max_effect),
            n_obs = n()) %>%
  filter(., n_obs >= 55)

effect_long <- pivot_longer(effect_wide, mean_OTC_effect:max_OTC_effect, names_to = 'var', values_to = 'values')

#Plotting ----
facet_labels <- as_labeller(c('max_OTC_effect' = 'Daily maximum',
                              'mean_OTC_effect' = 'Daily average',
                              'min_OTC_effect' = 'Daily minimum'
))

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

ggplot(effect_long, aes(x = as.integer(year), y = values, color = subsite, fill = subsite))+
  geom_hline(yintercept = 0, size = 0.25, color = 'dark grey')+
  geom_smooth(alpha = 0.3, size = 0.75, show.legend = FALSE)+
  geom_point(alpha = 0.7, shape = 21, size = 2, color = 'black')+
  theme_bw()+
  scale_fill_manual(values = c('#EEE191','#82AAD9'), name = 'Moisture class')+
  scale_color_manual(values = c('#EEE191','#82AAD9'), name = 'Moisture class')+
  ylab('Average OTC effect (°C)')+
  xlab('Year')+
  ggtitle('Effect of OTC on summer GST by soil moisutre class')+
  plot_theme+
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018,2019, 2020),
                     labels = c(2012, '', 2014, '', 2016, '', 2018, '', 2020))+
                     #limits = c(2012, 2018))+
  facet_wrap(~factor(var, levels = c("min_OTC_effect", "mean_OTC_effect", "max_OTC_effect")), labeller = facet_labels)+
  theme(panel.spacing.x = unit(0.75, "lines"))

#3 x 6.5

#Summary values --- 
effect_summary <- effect_long %>%
  group_by(year, subsite, var) %>%
  summarise(mean_effect = mean(values),
            sd_effect = sd(values),
            n_pairs = n())

effect_summary_all <- filter(effect_long, year < 2019) %>%
  group_by(subsite, var) %>%
  summarise(mean_effect = mean(values),
            sd_effect = sd(values),
            n_pairs = n())






