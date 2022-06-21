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

ggplot(moisture_summary, aes(x = factor(year), y = mean_moisture, fill = subsite))+
  geom_boxplot(size = .3, outlier.size = 0.75)+
  theme_pubr(base_size = 10, legend = 'bottom')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_jco(alpha = 0.8)+
  labs(y = 'Percent soil moisture',
       x = 'Year')+
  facet_wrap(~treatment)

#4x4
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



