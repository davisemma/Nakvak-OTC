#Script to test change in the abundance (total encounters per plot) 
#of the various lifeforms.
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
  filter(plot != '8A' & plot != '8B')

#Create a dataframe for each lifeform
gram <- filter(encounters_merge, lifeform == 'GRAM')
sdeci <- filter(encounters_merge, lifeform == 'SDECI')
sever <- filter(encounters_merge, lifeform == 'SEVER')
lichen <- filter(encounters_merge, lifeform == 'LICHEN')
forb <- filter(encounters_merge, lifeform == 'FORB')
bryo <- filter(encounters_merge, lifeform == 'BRYOPHYTE')

#Plotting format -
plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8,),
        plot.title = element_text(size = 10, vjust = 2, face = 'bold', margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(colour = 'black', size = 0),
        strip.text.x = element_text(size = 8, face = 'bold', hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        panel.border = element_rect(size = .4),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),)

#ABUNDANCE ANALYSIS ----
#GRAMINOIDS ----
#Plot of the distribution of observations
ggplot(gram, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")+
  plot_theme

#DRY - GRAM
dry_gram_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                   + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                   family = poisson, data = filter(gram, subsite == 'NAKVAKDRY'))

check_zeroinflation(dry_gram_p_mod)
check_overdispersion(dry_gram_p_mod)
check_model(dry_gram_p_mod) #over dispersed and minor zero inflation present

#ZAP model to see if dispersion due to zero's 
dry_gram_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                                 + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                                 family = truncated_poisson, data = filter(gram, subsite == 'NAKVAKDRY'))
check_zeroinflation(dry_gram_zap_mod) #worse zero inflation
check_overdispersion(dry_gram_zap_mod)
check_model(dry_gram_zap_mod)
#ZAP fixes dispersion but is a worse fit for zeros; try nbinom1 to address overdispersion

dry_gram_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(gram, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_gram_nb_mod)
check_zeroinflation(dry_gram_nb_mod)
check_model(dry_gram_nb_mod)

#Model comparison; ZAP and NB 
AICtab(dry_gram_p_mod, dry_gram_zap_mod, dry_gram_nb_mod)

#nb model possibly slight improvement over p model 
summary(dry_gram_nb_mod)
sjPlot::plot_model(dry_gram_nb_mod)

#WET - GRAM
wet_gram_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                   + (1|plot_pair/plot) + (1|year_scale),
                   family = poisson, data = filter(gram, subsite == 'NAKVAKWET'))

check_zeroinflation(wet_gram_p_mod) #no zeros 
check_overdispersion(wet_gram_p_mod) #data are overdispersed, no zeros

wet_gram_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = nbinom2, data = filter(gram, subsite == 'NAKVAKWET'))

check_overdispersion(wet_gram_nb_mod)
#nb model addresses overdispersion. Compare using AICtab out of curiosity 

AICtab(wet_gram_p_mod, wet_gram_nb_mod)
summary(wet_gram_nb_mod)

#LICHEN ----
#Plot of the distribution of observations
ggplot(lichen, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(year~subsite)+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")+
  plot_theme

#DRY - LICHEN
dry_lichen_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = poisson, data = filter(lichen, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_lichen_p_mod)
check_zeroinflation(dry_lichen_p_mod) #No zeros observed 

dry_lichen_nb_mod<- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                               family = nbinom1, data = filter(lichen, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_lichen_nb_mod)
check_zeroinflation(dry_lichen_nb_mod)

summary(dry_lichen_nb_mod)
sjPlot::plot_model(dry_lichen_nb_mod)
sjPlot::plot_model(dry_lichen_nb_mod, type = "int")

#WET - LICHEN
#Zero inflation definitely an issue in wet data, so choose between poiss and nbino hurdles
wet_lichen_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                                   + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                                   family = poisson, data = filter(lichen, subsite == 'NAKVAKWET'))

check_overdispersion(wet_lichen_p_mod) #data are under-dispersed
check_zeroinflation(wet_lichen_p_mod) #zero inflation not an issue

summary(wet_lichen_p_mod) 

sjPlot::plot_model(wet_lichen_p_mod, type = 'int')
#Overall, wet lichen model is probably not great because so few observations overall

#SDECI ----
#Plot of the distribution of observations
ggplot(sdeci, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  plot_theme+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")

#SDECI - DRY
dry_sdeci_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(sdeci, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sdeci_p_mod) #Not overdispersed
check_zeroinflation(dry_sdeci_p_mod) #No zeros

summary(dry_sdeci_p_mod)

#SDECI - WET
wet_sdeci_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = poisson, data = filter(sdeci, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sdeci_p_mod)
check_zeroinflation(wet_sdeci_p_mod) #no zeros in data, try nb model

wet_sdeci_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(sdeci, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sdeci_nb_mod)

summary(wet_sdeci_nb_mod)

sjPlot::plot_model(wet_sdeci_nb_mod)
sjPlot::plot_model(wet_sdeci_nb_mod, type = 'int')


#FORB ----
#Plot of the distribution of observations
ggplot(forb, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  theme_bw()

#DRY - FORB
dry_forb_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = poisson, data = filter(forb, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_forb_p_mod)
check_zeroinflation(dry_forb_p_mod)

#poisson good to go!
summary(dry_forb_p_mod)

#WET - FORB
wet_forb_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = poisson, data = filter(forb, subsite == 'NAKVAKWET'))
check_overdispersion(wet_forb_p_mod)
check_zeroinflation(wet_forb_p_mod) #small absolute difference in zeros

wet_forb_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                                  + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                                  family = truncated_poisson, data = filter(forb, subsite == 'NAKVAKWET'))

check_overdispersion(wet_forb_zap_mod)#doesn't fix dispersion
check_zeroinflation(wet_forb_zap_mod)#doesn't fix inflation

wet_forb_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                               family = nbinom1, data = filter(forb, subsite == 'NAKVAKWET'))

check_overdispersion(wet_forb_nb_mod)
check_zeroinflation(wet_forb_nb_mod) #check 0 inflated model

wet_forb_zanb_mod <- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                               family = truncated_nbinom1, data = filter(forb, subsite == 'NAKVAKWET'))

check_zeroinflation(wet_forb_zanb_mod)

AICtab(wet_forb_p_mod, wet_forb_nb_mod,
       wet_forb_zap_mod, wet_forb_zanb_mod)

summary(wet_forb_nb_mod)
summary(wet_forb_zanb_mod)
sjPlot::plot_model(wet_forb_nb_mod)
sjPlot::plot_model(wet_forb_nb_mod, type = "int")


#SEVER 
#Plot of the distribution of observations
ggplot(sever, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(year~subsite)+
  theme_bw()

#DRY - SEVER
dry_sever_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                          family = poisson, data = filter(sever, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sever_p_mod)
check_zeroinflation(dry_sever_p_mod) 

dry_sever_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                           family = truncated_poisson, data = filter(sever, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sever_zap_mod)
check_zeroinflation(dry_sever_zap_mod) #small zero inflation possibility

dry_sever_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(sever, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sever_nb_mod)
check_zeroinflation(dry_sever_nb_mod) #small zero inflation possibility

dry_sever_zanb_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                            family = truncated_nbinom1, data = filter(sever, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sever_zanb_mod)
check_zeroinflation(dry_sever_zanb_mod) #small zero inflation possibility

AICtab(dry_sever_p_mod, dry_sever_nb_mod,
       dry_sever_zap_mod, dry_sever_zanb_mod)

summary(dry_sever_nb_mod)

sjPlot::plot_model(dry_sever_nb_mod, type = 'int')
sjPlot::plot_model(dry_sever_zanb_mod, type = 'int')

#WET - SEVER
wet_sever_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(sever, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sever_p_mod)
check_zeroinflation(wet_sever_p_mod)

summary(wet_sever_p_mod)
sjPlot::plot_model(wet_sever_p_mod)
sjPlot::plot_model(wet_sever_p_mod, type = 'int')

#BRYOPHYTE
#Dry bryophyte ---
dry_bryo_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                          family = poisson, data = filter(bryo, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_bryo_p_mod)
check_zeroinflation(dry_bryo_p_mod)


dry_bryo_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(bryo, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_bryo_nb_mod) #improved dispersion
check_zeroinflation(dry_bryo_nb_mod) #A bit worse zero modelling

sjPlot::plot_model(dry_bryo_nb_mod, type = 'int')
summary(dry_bryo_nb_mod)

#WET bryophyte models
wet_bryo_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = poisson, data = filter(bryo, subsite == 'NAKVAKWET'))

check_overdispersion(wet_bryo_p_mod)
check_zeroinflation(wet_bryo_p_mod)


wet_bryo_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = nbinom1, data = filter(bryo, subsite == 'NAKVAKWET'))

check_overdispersion(wet_bryo_nb_mod) #improved dispersion
check_zeroinflation(wet_bryo_nb_mod) #A bit worse zero modelling

sjPlot::plot_model(wet_bryo_nb_mod, type = 'int')
summary(wet_bryo_nb_mod)


#READ DATA ----
holders_ground <- filter(holders, lifeform == 'FORB' | lifeform == 'GRAM') %>%
  mutate(lifeform = if_else(lifeform == 'FORB', 'LITTER', 'NON'))

#FROMAT DATA ----
#Calculate n encounters for each of 6 lifeforms per plot
encounters_ground <- data %>% 
  filter(., lifeform == 'LITTER' | lifeform == 'BARE' | lifeform == 'SOIL') %>% #select only 'live' encounters
  filter(., year != 2008) %>% #remove 2008 observations
  mutate(lifeform = if_else(lifeform == 'BARE', 'NON', if_else(lifeform == 'SOIL', 'NON', lifeform)))%>%
  group_by(subsite, treatment, plot, year, lifeform) %>% 
  summarise(encounters = n()) %>%
  filter(plot != '8A' & plot != '8B')

#Merge encounters with placeholder file, replace 'NA' with '0', format
encounters_ground_merge <- merge(encounters_ground, holders_ground, all.y = TRUE) %>% #merge w 'holders' so that all combinations are represented (i.e., '0's are meaningful and should be included)
  mutate(encounters = replace_na(encounters, 0), #NA changed to '0'
         subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year)) #converted year to z-score for modelling

#Create a dataframe for each lifeform
plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8,),
        plot.title = element_text(size = 10, vjust = 2, face = 'bold', margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(colour = 'black', size = 0),
        strip.text.x = element_text(size = 8, face = 'bold', hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        panel.border = element_rect(size = .4),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),)

#ABUNDANCE ANALYSIS ----
#Plot of the distribution of observations
bare <- filter(encounters_ground_merge, lifeform == 'NON')
litter <- filter(encounters_ground_merge, lifeform == 'LITTER')

#LITTER
ggplot(litter, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 5)+
  facet_grid(year~subsite)+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")+
  plot_theme

dry_litter_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(litter, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_litter_p_mod)

dry_litter_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(litter, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_litter_nb_mod)

summary(dry_litter_nb_mod)
plot_model(dry_litter_nb_mod, type = 'int')

#WET PLOT LITTER
wet_litter_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(litter, subsite == 'NAKVAKWET'))

check_overdispersion(wet_litter_p_mod)

wet_litter_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(litter, subsite == 'NAKVAKWET'))

check_overdispersion(wet_litter_nb_mod)
summary(wet_litter_nb_mod)
plot_model(wet_litter_nb_mod, type = 'int')

#BARE
ggplot(bare, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 2)+
  facet_grid(year~subsite)+
  scale_fill_manual(values = c("plum1", 'seagreen3'), name = "Treatment")+
  plot_theme

dry_bare_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                          family = poisson, data = filter(bare, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_bare_p_mod)

dry_bare_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(bare, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_bare_nb_mod)

summary(dry_bare_nb_mod)
plot_model(dry_bare_nb_mod, type = 'int')

#WET PLOT 
wet_bare_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                          family = poisson, data = filter(bare, subsite == 'NAKVAKWET'))

check_overdispersion(wet_bare_p_mod)
check_model(wet_bare_p_mod)
summary(wet_bare_p_mod)
plot_model(wet_bare_p_mod, type = 'int')


