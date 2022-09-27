#Script to test change in MEAN height of lifeforms
rm(list=ls())
#Load packages ----
library(dplyr)
library(tidyr)
library(tidylog)
library(ggplot2)
library(ggsci)
library(ggthemes)
library(sjPlot)
library(stringr)
#install.packages("glmmTMB", type = "source")
library(glmmTMB)
library(performance)
library(bbmle)
library(cowplot)

#READ DATA ----
data <- read.csv("Point Frame/plot_data_QC_ELD.csv") 
holders <- read.csv("Point Frame/plot_year_combination.csv") %>%
  filter(., year != 2008)#file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate mean encounters for each lifeform x plot
mean_height <- data %>%
  filter(., status == "LIVE") %>%
  filter(., year != 2008) %>%
  mutate(height = replace_na(height, 0)) %>%
  group_by(subsite, treatment, plot, year, lifeform) %>%
  summarise(mean_height = mean(height, na.rm = TRUE)) 

#Merge with placeholder data
mean_height_merge <- merge(mean_height, holders, all.y = TRUE) %>%
  mutate(subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year),
         mean_height = replace_na(mean_height, 0)) %>%
  filter(plot != '8A' & plot != '8B')

#Create a dataframe for each lifeform
gram <- filter(mean_height_merge, lifeform == 'GRAM')
sdeci <- filter(mean_height_merge, lifeform == 'SDECI')
sever <- filter(mean_height_merge, lifeform == 'SEVER')
forb <- filter(mean_height_merge, lifeform == 'FORB')

#Set theme for plots
plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -10, l = -6, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 10, vjust = 1, face = 'bold', margin = margin(t = 2, unit = 'pt')),
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

#AVERAGE HEIGHT ANALYSIS ----
#GRAMINOIDS ----
#Plot of the distribution of observations
ggplot(gram, aes(x = as.factor(year), y = mean_height, fill = treatment)) +
  geom_boxplot()+
  #geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(~subsite)+
  plot_theme

#DRY - GRAM
dry_gram_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKDRY'))
summary(dry_gram_mod)
check_model(dry_gram_mod)

#WET - GRAM
wet_gram_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKWET'))
summary(wet_gram_mod)
check_model(wet_gram_mod)
sjPlot::plot_model(wet_gram_mod)
sjPlot::plot_model(wet_gram_mod, type = "int")

#SDECI ----
#Plot of the distribution of observations
ggplot(sdeci, aes(x = as.factor(year), y = mean_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()

#DRY - SDECI
dry_sdeci_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sdeci, subsite == 'NAKVAKDRY'))
summary(dry_sdeci_mod)
check_model(dry_sdeci_mod)
sjPlot::plot_model(dry_sdeci_mod)
sjPlot::plot_model(dry_sdeci_mod, type = "int")
#WET - SDECI
wet_sdeci_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sdeci, subsite == 'NAKVAKWET'))
summary(wet_sdeci_mod)
check_model(wet_sdeci_mod)
sjPlot::plot_model(wet_sdeci_mod)

#SEVER ----
#Plot of the distribution of observations
ggplot(sever, aes(x = as.factor(year), y = mean_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()


#DRY - SEVER
dry_sever_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sever, subsite == 'NAKVAKDRY'))
summary(dry_sever_mod)
check_model(dry_sever_mod)
sjPlot::plot_model(dry_sever_mod)
#WET - SEVER
wet_sever_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sever, subsite == 'NAKVAKWET'))
summary(wet_sever_mod)
check_model(wet_sever_mod)
sjPlot::plot_model(wet_sever_mod)

#FORB ----
#Plot of the distribution of observations
ggplot(forb, aes(x = as.factor(year), y = mean_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()

#DRY - FORB
dry_forb_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(forb, subsite == 'NAKVAKDRY'))
summary(dry_forb_mod)
check_model(dry_forb_mod)
sjPlot::plot_model(dry_forb_mod)

#WET - FORB
wet_forb_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(forb, subsite == 'NAKVAKWET'))
summary(wet_forb_mod)
check_model(wet_forb_mod)
sjPlot::plot_model(wet_forb_mod)
sjPlot::plot_model(wet_forb_mod, type = "int")

#CANOPY HEIGHT 
#READ DATA ----
data <- read.csv("Point Frame/plot_data_QC_ELD.csv") 
holders <- read.csv("Point Frame/plot_year_combination.csv") %>%
  filter(year != 2008)#file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate canopy height plot
canopy_height <- data %>%
  filter(., status == "LIVE") %>%
  filter(year != 2008) %>%
  mutate(height = replace_na(height, 0)) %>%
  filter(., hit_order == 1) %>%
  group_by(subsite, treatment, plot, year) %>%
  summarise(max_height = max(height),
            mean_height = mean(height)) %>%
  mutate(subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         year_scale = scale(year, center = TRUE)) %>%
  filter(plot != '8A' & plot!= '8B')

#CANOPY HEIGHT ANALYSIS ----
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
        axis.ticks.length = unit(1.5, "pt"),)

#Plot of the distribution of observations
ggplot(canopy_height, aes(x = as.factor(year), y = max_height, fill = treatment)) +
  geom_boxplot()+
  #geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(~subsite)+
  plot_theme+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")


#DRY
dry_canopy_mod <- glmmTMB(max_height ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale),
                          family = gaussian, data = filter(canopy_height, subsite == 'NAKVAKDRY'))
summary(dry_canopy_mod)
diagnose(dry_canopy_mod)
check_model(dry_canopy_mod)
#*NOTE DIFFERENT family

#WET
wet_canopy_mod <- glmmTMB(mean_height ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale),
                          family = gaussian, data = filter(canopy_height, subsite == 'NAKVAKWET'))
summary(wet_canopy_mod)
check_model(wet_canopy_mod)

sjPlot::plot_model(wet_canopy_mod, type = "int")
sjPlot::plot_model(wet_canopy_mod)

