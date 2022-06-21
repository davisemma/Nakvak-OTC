#Code to plot formatted soil moisture data - 2009 to 2013
#LAST UPDATE: April 27, 2020
rm(list = ls())

setwd("~/Desktop/Labrador Project/Nakvak Soil Moisture")
soil_moisture <- read_csv("Nakvak_SoilMoisture_Formatted.csv")
soil_moisture$Subsite <-  ordered(soil_moisture$Subsite, levels = c("Wet", "Dry"))

moisture_summary <- soil_moisture %>%
  group_by(Site, Year, Subsite, Treatment, PlotID) %>%
  summarise(MeanMoist = mean(Moisture, na.rm = TRUE))

n_obs <- moisture_summary %>%
  group_by(Site, Year, Subsite, Treatment) %>%
  summarise(Obs = n())

ggplot(moisture_summary, aes(x = factor(Year), y = MeanMoist, fill = Subsite))+
  geom_boxplot(size = .3, outlier.size = 0.75)+
  theme_pubr(base_size = 10, legend = 'bottom')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_jco(alpha = 0.8)+
  labs(y = 'Percent soil moisture',
       x = 'Year')

#4x4
moisture_summary$PlotID = factor(moisture_summary$PlotID)
moisture_summary$Subsite = factor(moisture_summary$Subsite)
moisture_summary$Year = factor(moisture_summary$Year)
moisture.mod <- lmer(MeanMoist ~ Subsite*Year + (1|PlotID), data = moisture_summary)
summary(moisture.mod)
plot(moisture.mod)
qqnorm(resid(moisture.mod))
qqline(resid(moisture.mod))

moist.con <- emmeans(moisture.mod , ~Year|Subsite)
contrast(moist.con, "poly", "tukey")  


#
ggplot(moisture_summary, aes(x = Year, y = MeanMoist, colour = Subsite))+
  geom_point()




