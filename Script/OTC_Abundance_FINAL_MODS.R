#CODE FOR RUNNING ABUNDANCE MODELS ONLY ----
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

getwd()

#READ DATA ----
data <- read.csv("Point Frame/plot_data_QC_ELD.csv") 
holders <- read.csv("Point Frame/plot_year_combination.csv") %>%
  filter(., year != 2008)#file with all lifeform x plot combinations; remove 2008

#FROMAT DATA ----
#Calculate n encounters for each of 6 lifeforms per plot
encounters <- data %>% 
  filter(., status == 'LIVE') %>% #select only 'live' encounters
  filter(., year != 2008) %>% #remove 2008 observations
  mutate(lifeform = if_else(lifeform == "MOSS", "BRYOPHYTE", if_else(lifeform == "LIVERWORT", "BRYOPHYTE", lifeform))) %>%
  group_by(subsite, treatment, plot, year, lifeform) %>% #liverworts and mosses become 'bryophytes'
  summarise(encounters = n()) #number of encounters of each lifeform in each plot

#Merge encounters with placeholder file, replace 'NA' with '0', format
encounters_merge <- merge(encounters, holders, all.y = TRUE) %>% #merge w 'holders' so that all combinations are represented (i.e., '0's are meaningful and should be included)
  mutate(encounters = replace_na(encounters, 0), #NA changed to '0'
         subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year)) %>%
  filter(plot != '8A' & plot!= '8B')


#Format data for ground observaitons ----
holders_ground <- filter(holders, lifeform == 'FORB' | lifeform == 'GRAM') %>%
  mutate(lifeform = if_else(lifeform == 'FORB', 'LITTER', 'NON'))

encounters_ground <- data %>% 
  filter(., lifeform == 'LITTER' | lifeform == 'BARE' | lifeform == 'SOIL') %>% #select only 'live' encounters
  filter(., year != 2008) %>% #remove 2008 observations
  mutate(lifeform = if_else(lifeform == 'BARE', 'NON', if_else(lifeform == 'SOIL', 'NON', lifeform)))%>%
  group_by(subsite, treatment, plot, year, lifeform) %>% 
  summarise(encounters = n()) #number of encounters of each lifeform in each plot

encounters_ground_merge <- merge(encounters_ground, holders_ground, all.y = TRUE) %>% #merge w 'holders' so that all combinations are represented (i.e., '0's are meaningful and should be included)
  mutate(encounters = replace_na(encounters, 0), #NA changed to '0'
         subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year)) %>%
  filter(plot != '8A' & plot!= '8B')

#Create a dataframe for each lifeform and litter 
gram <- filter(encounters_merge, lifeform == 'GRAM')
sdeci <- filter(encounters_merge, lifeform == 'SDECI')
sever <- filter(encounters_merge, lifeform == 'SEVER')
lichen <- filter(encounters_merge, lifeform == 'LICHEN')
forb <- filter(encounters_merge, lifeform == 'FORB')
bryo <- filter(encounters_merge, lifeform == 'BRYOPHYTE')
litter <- filter(encounters_ground_merge, lifeform == 'LITTER')


#Model formulations ----
#DRY MODELS ----
dry_gram_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(gram, subsite == 'NAKVAKDRY'))
dry_lichen_nb_mod<- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = nbinom1, data = filter(lichen, subsite == 'NAKVAKDRY'))
dry_sdeci_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = poisson, data = filter(sdeci, subsite == 'NAKVAKDRY'))
dry_forb_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                          family = poisson, data = filter(forb, subsite == 'NAKVAKDRY'))
dry_sever_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = nbinom1, data = filter(sever, subsite == 'NAKVAKDRY'))
dry_bryo_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(bryo, subsite == 'NAKVAKDRY'))
dry_litter_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(litter, subsite == 'NAKVAKDRY'))
#WET MODELS ----
wet_gram_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale),
                           family = nbinom2, data = filter(gram, subsite == 'NAKVAKWET'))
wet_lichen_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(lichen, subsite == 'NAKVAKWET'))
wet_sdeci_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = nbinom1, data = filter(sdeci, subsite == 'NAKVAKWET'))
wet_forb_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(forb, subsite == 'NAKVAKWET'))
wet_sever_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = poisson, data = filter(sever, subsite == 'NAKVAKWET'))
wet_bryo_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(bryo, subsite == 'NAKVAKWET'))
wet_litter_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(litter, subsite == 'NAKVAKWET'))



#List of final models 
dry_gram_nb_mod
dry_lichen_nb_mod
dry_sdeci_p_mod
dry_forb_p_mod
dry_sever_nb_mod
dry_bryo_nb_mod
dry_litter_nb_mod

wet_gram_nb_mod
wet_lichen_p_mod
wet_sdeci_nb_mod
wet_forb_nb_mod
wet_sever_p_mod
wet_bryo_nb_mod
wet_litter_nb_mod

