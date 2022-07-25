library(ggpubr)
#Code to plot formatted soil moisture data 
rm(list = ls())

soil_moisture <- read_csv("Soil Moisture/Torngat_2021_TempSoil_DATA_FORMATTED.csv") %>%
  filter(!is.na(treatment))
soil_moisture$Subsite <-  ordered(soil_moisture$subsite, levels = c("Wet", "Dry"))

moisture_summary <- soil_moisture %>%
  group_by(site, year, subsite, treatment, plot) %>%
  summarise(mean_moisture = mean(moisture, na.rm = TRUE))

n_obs <- moisture_summary %>%
  filter(!is.nan(mean_moisture)) %>%
  group_by(site, year, subsite, treatment) %>%
  summarise(Obs = n())

facet_labels <- as_labeller(c('Dry' = 'Dry plots', 'Wet' = 'Wet plots'))

plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
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
        strip.text.x = element_text(size = 8, face = 'bold', hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        panel.border = element_rect(size = .4),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"))

ggplot(moisture_summary, aes(x = year, y = mean_moisture, group = interaction(treatment, year), fill = treatment))+
  ggtitle('Instantaneous soil moisture measurements by year')+
  geom_boxplot(size = 0.3, outlier.size = 0.3)+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")+
  labs(y = '% Soil moisture',
       x = '')+
  scale_x_continuous(breaks=seq(2009, 2021, 2))+
  facet_wrap(~subsite, scales = 'free_y', labeller = facet_labels)+
  plot_theme

#3 x 6 inch export size 

model_dat <- moisture_summary %>%
  ungroup()%>%
  mutate(plot_pair = factor(paste(treatment,plot, sep = "")),
         plot = factor(plot),
         subsite = factor(subsite),
         treament = factor(treatment),
         year_scale = as.numeric(scale(year)))

wet_moisture <- glmmTMB(mean_moisture ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(model_dat, subsite == 'Wet'))

summary(wet_moisture)
check_model(wet_moisture)

dry_moisture <- glmmTMB(mean_moisture ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(model_dat, subsite == 'Dry'))

summary(dry_moisture)
check_model(dry_moisture)

sjPlot::plot_model(wet_moisture)
sjPlot::plot_model(wet_moisture, type = "int")

sjPlot::plot_model(dry_moisture)
sjPlot::plot_model(dry_moisture, type = "int")



