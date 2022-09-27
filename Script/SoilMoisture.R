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

facet_labels <- as_labeller(c('Dry' = '', 'Wet' = ''))

plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
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
        axis.ticks.length = unit(1.5, "pt"))

moisture_plot <- ggplot(moisture_summary, aes(x = year, y = mean_moisture, group = interaction(treatment, year), fill = interaction(treatment, subsite)))+
  geom_boxplot(size = .2, outlier.size = 0.5, position = position_dodge2(preserve = "single", padding = 0))+
  scale_fill_manual(values = c("#F0DF84", '#F6F1D1', '#82AAD9', '#C2D8F2'))+
  labs(y = '% Soil moisture',
       x = 'Year')+
  scale_x_continuous(breaks=seq(2008, 2022, 1),
                     labels = c("", 2009, "", 2011, "", 2013, "", 2015, "", 2017, "", 2019, "", 2021, ""),
                     limits = c(2008, 2022))+
  facet_wrap(~subsite, scales = 'fixed', labeller = facet_labels_NVO) +
  plot_theme+
  theme(legend.position= "none")+
  theme(plot.margin = margin(0, 6, 0, 6))+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

moisture_plot

fake <- ggplot(moisture_summary, aes(x = year, y = mean_moisture, group = interaction(treatment, year), fill = treatment))+
  #ggtitle('Instantaneous soil moisture measurements by year')+
  geom_boxplot(size = 0.3, outlier.size = 0.3)+
  #scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")+
  scale_fill_manual(values = c("#BDBEBF", '#F0F1F3'), name = 'Treatment',
                    labels = c('Control', 'OTC'))+
  labs(y = '% Soil moisture',
       x = '')+
  scale_x_continuous(breaks=seq(2009, 2021, 1),
                     labels = c(2009, "", 2011, "", 2013, "", 2015, "", 2017, "", 2019, "", 2021),
                     limits = c(2009, 2021))+
  facet_wrap(~subsite, scales = 'fixed', labeller = 'facet_labels')+
  plot_theme


legend <- get_legend(fake + theme(legend.position = 'top',
                                  legend.box.margin = margin(0, 0, 3, 0),
                                  legend.text = element_text(size = 8),
                                  legend.title = element_text(size = 8),
                                  legend.justification = "left"))

cowplot::plot_grid(legend, NVO_plot, moisture_plot, ncol = 1, rel_heights = c(0.15,1.15, 1),
                   align = c("v"),
                   axis = c("l"),
                   labels = c("", "A", "B"),
                   label_x = -.001,
                   label_y = 1.02,
                   label_size = 10)
#5x6

#3 x 6 inch export size 

model_dat <- moisture_summary %>%
  ungroup()%>%
  mutate(plot_pair = factor(paste(treatment,plot, sep = "")),
         plot = factor(plot),
         subsite = factor(subsite),
         treament = factor(treatment),
         year_scale = as.numeric(scale(year)))

wet_moisture <- glmmTMB(mean_moisture ~ treatment*year_scale
                        + (1|plot) + (1|year_scale),
                        family = gaussian, data = filter(model_dat, subsite == 'Wet'))

summary(wet_moisture)
check_model(wet_moisture)

dry_moisture <- glmmTMB(mean_moisture ~ treatment*year_scale
                        + (1|plot) + (1|year_scale),
                        family = gaussian, data = filter(model_dat, subsite == 'Dry'))

summary(dry_moisture)
check_model(dry_moisture)

all_moisture <- glmmTMB(mean_moisture ~ subsite*year_scale
                        + (1|plot) + (1|year_scale),
                        family = gaussian, data = model_dat)

summary(all_moisture)
check_model(all_moisture)

sjPlot::tab_model(dry_moisture,
                  wet_moisture,
                  all_moisture)

sjPlot::plot_model(wet_moisture)
sjPlot::plot_model(wet_moisture, type = "int")

sjPlot::plot_model(dry_moisture)
sjPlot::plot_model(dry_moisture, type = "int")

summary_measurements <- moisture_summary %>%
  group_by(subsite) %>%
  summarise(mean_moisture_all = mean(mean_moisture, na.rm = TRUE),
            sd_moistutre_all = sd(mean_moisture, na.rm = TRUE))

summary_measurements_yr <- moisture_summary %>%
  group_by(year,subsite) %>%
  summarise(mean_moisture_yr = mean(mean_moisture, na.rm = TRUE),
            sd_moistutre_yr = sd(mean_moisture, na.rm = TRUE))

