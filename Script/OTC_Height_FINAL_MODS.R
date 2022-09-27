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
  summarise(mean_height = mean(height)) 

#Merge with placeholder data
mean_height_merge <- merge(mean_height, holders, all.y = TRUE) %>%
  mutate(subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year),
         mean_height = replace_na(mean_height, 0))%>%
  filter(plot != '8A' & plot != '8B') 

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
         year_scale = scale(year, center = TRUE))%>%
  filter(plot != '8A' & plot != '8B')

#Create a dataframe for each lifeform
gram <- filter(mean_height_merge, lifeform == 'GRAM')
sdeci <- filter(mean_height_merge, lifeform == 'SDECI')
sever <- filter(mean_height_merge, lifeform == 'SEVER')
forb <- filter(mean_height_merge, lifeform == 'FORB')



dry_gram_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKDRY'))
dry_sdeci_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sdeci, subsite == 'NAKVAKDRY'))
dry_sever_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sever, subsite == 'NAKVAKDRY'))
dry_forb_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(forb, subsite == 'NAKVAKDRY'))
dry_canopy_mod <- glmmTMB(max_height ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale),
                          family = gaussian, data = filter(canopy_height, subsite == 'NAKVAKDRY'))


wet_gram_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKWET'))
wet_sdeci_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sdeci, subsite == 'NAKVAKWET'))
wet_sever_mod <- glmmTMB(mean_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sever, subsite == 'NAKVAKWET'))
wet_forb_mod <- glmmTMB(mean_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(forb, subsite == 'NAKVAKWET'))
wet_canopy_mod <- glmmTMB(max_height ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale),
                          family = gaussian, data = filter(canopy_height, subsite == 'NAKVAKWET'))

#LIST OF MODEL OUTPUTS
sjPlot::tab_model(dry_gram_mod,
                  dry_sdeci_mod,
                  dry_sever_mod,
                  dry_forb_mod,
                  dry_canopy_mod)

sjPlot::tab_model(wet_gram_mod,
                  wet_sdeci_mod,
                  wet_sever_mod,
                  wet_forb_mod,
                  wet_canopy_mod)
                  
                  