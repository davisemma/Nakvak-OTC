#Script to test change in maximum canopy height of plots
rm(list=ls())
#Load packages ----
library(dplyr)
library(tidyr)
library(tidylog)
library(ggplot2)
library(ggsci)
library(ggthemes)
library(stringr)
#install.packages("glmmTMB", type = "source")
library(glmmTMB)
library(performance)
library(bbmle)

#READ DATA ----
data <- read.csv("Point Frame/plot_data_fin.csv") 
holders <- read.csv("Point Frame/plot_year_genus_fin.csv") #file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate canopy height plot
canopy_height <- data %>%
  filter(., status == "LIVE") %>%
  mutate(height = replace_na(height, 0)) %>%
  filter(., hit_order == 1) %>%
  group_by(subsite, treatment, plot, year) %>%
  summarise(max_height = max(height),
            mean_height = mean(height)) %>%
  mutate(subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         year_scale = scale(year)) #converted to z-score for modelling

#CANOPY HEIGHT ANALYSIS ----
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
        axis.ticks.length = unit(1.5, "pt"),)

#Plot of the distribution of observations
ggplot(canopy_height, aes(x = as.factor(year), y = max_height, fill = treatment)) +
  geom_boxplot()+
  #geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(~subsite)+
  plot_theme+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")

canopy_subsite_mod <- glmmTMB(max_height ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = gaussian, data = canopy_height) 

check_model(canopy_subsite_mod) #seems to be a good fit
summary(canopy_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

#DRY
dry_canopy_mod <- glmmTMB(max_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(canopy_height, subsite == 'NAKVAKDRY'))
summary(dry_canopy_mod)
check_model(dry_canopy_mod)

#WET
wet_canopy_mod <- glmmTMB(max_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(canopy_height, subsite == 'NAKVAKWET'))
summary(wet_canopy_mod)
check_model(wet_canopy_mod)
sjPlot::plot_model(wet_canopy_mod, type = "int")
sjPlot::plot_model(wet_canopy_mod)
