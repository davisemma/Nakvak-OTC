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
data <- read.csv("Point Frame/plot_data_fin.csv") 
holders <- read.csv("Point Frame/plot_year_genus_fin.csv") #file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate n encounters for each lifeform x plot
encounters <- data %>% 
  filter(., status == 'LIVE') %>% #select only 'live' encounters
  group_by(subsite, treatment, plot, year, lifeform) %>%
  summarise(encounters = n()) #number of encounters of each lifeform in each plot

#Merge encounters with placeholder file, replace 'NA' with '0', format
encounters_merge <- merge(encounters, holders, all.y = TRUE) %>% #merge w 'holders' so that all combinations are represented ('0's are meaningful and should be included)
  mutate(encounters = replace_na(encounters, 0), #NA changed to '0'
         subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year)) #converted year to z-score for modelling

#Create a dataframe for each lifeform
gram <- filter(encounters_merge, lifeform == 'GRAM')
sdeci <- filter(encounters_merge, lifeform == 'SDECI')
sever <- filter(encounters_merge, lifeform == 'SEVER')
lichen <- filter(encounters_merge, lifeform == 'LICHEN')
forb <- filter(encounters_merge, lifeform == 'FORB')
moss <- filter(encounters_merge, lifeform == 'MOSS')
liverwort <- filter(encounters_merge, lifeform == 'LIVERWORT')

#ABUNDANCE ANALYSIS ----
#GRAMINOIDS ----
#Plot of the distribution of observations
ggplot(gram, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  theme_bw()

#Dry and wet seem to have very different distributions; test whether they could be modelled separately
gram_subsite_mod <- glmmTMB(encounters ~ subsite
               + (1|plot_pair/plot) + (1|year_scale),
               family = nbinom1, data = gram) #nbinom1 best fit 

check_model(gram_subsite_mod)
summary(gram_subsite_mod)
#Subsite has a strong, significant effect. 
#Now will model counts separately for wet vs dry, deciding between model types along the way

#DRY - GRAM
dry_gram_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                   + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                   family = poisson, data = filter(gram, subsite == 'NAKVAKDRY'))

check_zeroinflation(dry_gram_p_mod)
check_overdispersion(dry_gram_p_mod)
#overdispersion AND zero inflation present
#RUN ZAP
dry_gram_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                                 + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                                 family = truncated_poisson, data = filter(gram, subsite == 'NAKVAKDRY'))
check_zeroinflation(dry_gram_zap_mod)
check_overdispersion(dry_gram_zap_mod)
#ZAP fixes dispersion but is a worse fit for zeros; try nbinom1 to address overdispersion
dry_gram_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(gram, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_gram_nb_mod)
check_zeroinflation(dry_gram_nb_mod)
#nbinom1 fixes overdispersion and is a better fit for 0's. Compare models 
#Running ZANB model just to see how different 
dry_gram_zanb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                           family = truncated_nbinom1, data = filter(gram, subsite == 'NAKVAKDRY'))

#Model comparison; ZAP and NB 
AICtab(dry_gram_p_mod, dry_gram_nb_mod,
       dry_gram_zap_mod, dry_gram_zanb_mod)
#nb model is most parsimonious
summary(dry_gram_nb_mod)
sjPlot::plot_model(dry_gram_nb_mod)

#WET - GRAM
wet_gram_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                   + (1|plot_pair/plot) + (1|year_scale),
                   family = poisson, data = filter(gram, subsite == 'NAKVAKWET'))

check_zeroinflation(wet_gram_p_mod) #no zeros 
check_overdispersion(wet_gram_p_mod)
#No indication of zero inflation in wet data, so choose between poiss and nbino

wet_gram_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = nbinom1, data = filter(gram, subsite == 'NAKVAKWET'))

check_overdispersion(wet_gram_nb_mod)
#nb model addresses overdispersion. Compare using AICtab out of curiosity 

AICtab(wet_gram_p_mod, wet_gram_nb_mod)
summary(wet_gram_nb_mod)

sjPlot::plot_model(dry_gram_nb_mod, type = 'int')
sjPlot::plot_model(dry_gram_nb_mod)
sjPlot::plot_model(wet_gram_nb_mod, type = 'int')
sjPlot::plot_model(wet_gram_nb_mod)

#LICHEN ----
#Plot of the distribution of observations
ggplot(lichen, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(year~subsite)+
  theme_bw()

#Dry and wet seem to have very different distributions; test whether they could be modelled separately
lichen_subsite_mod <- glmmTMB(encounters ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = nbinom1, data = lichen) #nbinom1 better fit than poisson

check_overdispersion(lichen_subsite_mod)
check_model(lichen_subsite_mod)
summary(lichen_subsite_mod)
#Subsite has strong, significant effect. Now will model counts separately for wet vs dry

#DRY - LICHEN
dry_lichen_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = poisson, data = filter(lichen, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_lichen_p_mod)
check_zeroinflation(dry_lichen_p_mod) #No zeros observed 

dry_lichen_nb_mod<- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                               family = nbinom2, data = filter(lichen, subsite == 'NAKVAKDRY'))
#***Note: nbinom2
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

wet_lichen_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                                    + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                                    family = nbinom1, data = filter(lichen, subsite == 'NAKVAKWET'))

check_overdispersion(wet_lichen_nb_mod)
check_zeroinflation(wet_lichen_nb_mod) #slight overfitting of zero's

wet_lichen_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                             family = truncated_poisson, data = filter(lichen, subsite == 'NAKVAKWET'))

wet_lichen_zinb_mod <- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                               family = truncated_nbinom1, data = filter(lichen, subsite == 'NAKVAKWET'))

AICtab(wet_lichen_p_mod, wet_lichen_nb_mod,
       wet_lichen_zap_mod, wet_lichen_zinb_mod) #
summary(wet_lichen_p_mod) 

sjPlot::plot_model(wet_lichen_p_mod, type = 'int')
#Overall, wet lichen model is probably not great because so few observations overall

#SDECI ----
#Plot of the distribution of observations
ggplot(sdeci, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  theme_bw()

#Dry and wet seem to have similar distributions; test whether they could be modelled separately
sdeci_subsite_mod <- glmmTMB(encounters ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = nbinom1, data = sdeci) #nbinom1 better fit than poisson

check_overdispersion(sdeci_subsite_mod)
summary(sdeci_subsite_mod)
#Subsite has weak, insignificant effect. Now will model counts together then separately for dry and wet

#ALL SUBSITE - SDECI
sdeci_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = poisson, data = sdeci)

check_overdispersion(sdeci_p_mod)
check_zeroinflation(sdeci_p_mod)
#overdispersion, weak underfitting of zeros; no visual zero inflation
sdeci_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = nbinom1, data = sdeci)

check_overdispersion(sdeci_nb_mod)

AICtab(sdeci_p_mod, sdeci_nb_mod)
summary(sdeci_nb_mod)

#SDECI - DRY
dry_sdeci_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(sdeci, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sdeci_p_mod) #Not really overdispersed
check_zeroinflation(dry_sdeci_p_mod) #No zeros

dry_sdeci_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                           + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                           family = nbinom1, data = filter(sdeci, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_sdeci_nb_mod)
check_zeroinflation(dry_sdeci_nb_mod) #weak underfitting of zeros

AICtab(dry_sdeci_p_mod, dry_sdeci_nb_mod)

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

#MOSS ----
#Plot of the distribution of observations
ggplot(moss, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  theme_bw()

#Dry and wet seem to have very distributions; test whether they could be modelled separately
moss_subsite_mod <- glmmTMB(encounters ~ subsite
                              + (1|plot_pair/plot) + (1|year_scale),
                              family = nbinom1, data = moss) #nbinom1 better fit than poisson

check_overdispersion(moss_subsite_mod)
summary(moss_subsite_mod)
#Subsite has strong, significant effect. Now will model counts separately for wet vs dry

#DRY - MOSS
dry_moss_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                               family = poisson, data = filter(moss, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_moss_p_mod)
check_zeroinflation(dry_moss_p_mod) 
#overdispersion detected; zero inflation slight issue 

dry_moss_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                          + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                          family = truncated_poisson, data = filter(moss, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_moss_zap_mod)
check_zeroinflation(dry_moss_zap_mod) #Doesn't address dispersion and worse fit for 0's

dry_moss_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                                 + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                                 family = nbinom1, data = filter(moss, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_moss_nb_mod)
check_zeroinflation(dry_moss_nb_mod)

dry_moss_zanb_mod <- glmmTMB(encounters ~ treatment*year_scale
                               + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                               family = truncated_nbinom1, data = filter(moss, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_moss_zanb_mod)
check_zeroinflation(dry_moss_zanb_mod)

AICtab(dry_moss_p_mod, dry_moss_nb_mod,
       dry_moss_zap_mod, dry_moss_zanb_mod)

summary(dry_moss_nb_mod)
sjPlot::plot_model(dry_moss_nb_mod, type = "int")
#WET - MOSS
wet_moss_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                                 + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                                 family = poisson, data = filter(moss, subsite == 'NAKVAKWET'))
check_overdispersion(wet_moss_p_mod)
check_zeroinflation(wet_moss_p_mod) #small absolute difference in zeros

wet_moss_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(moss, subsite == 'NAKVAKWET'))

check_overdispersion(wet_moss_nb_mod)
check_zeroinflation(wet_moss_nb_mod)

AICtab(wet_moss_p_mod, wet_moss_nb_mod)
summary(wet_moss_nb_mod)

sjPlot::plot_model(wet_moss_nb_mod, type = 'int', terms = 'year_scale')

#FORB ----
#Plot of the distribution of observations
ggplot(forb, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 10)+
  facet_grid(year~subsite)+
  theme_bw()

#Dry and wet seem to have very difference distributions; test whether they could be modelled separately
forb_subsite_mod <- glmmTMB(encounters ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = nbinom1, data = forb) #nbinom1 better fit than poisson

check_overdispersion(forb_subsite_mod)
summary(forb_subsite_mod)
#Subsite has strong, significant effect. Now will model counts separately for wet vs dry

#DRY - FORB
dry_forb_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = poisson, data = filter(forb, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_forb_p_mod)
check_zeroinflation(dry_forb_p_mod) #small zero inflation possibility

dry_forb_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                            family = truncated_poisson, data = filter(forb, subsite == 'NAKVAKDRY'))

check_overdispersion(dry_forb_zap_mod)
check_zeroinflation(dry_forb_zap_mod) #regular poisson seems better fit

AICtab(dry_forb_p_mod, dry_forb_zap_mod)
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

summary(wet_forb_zanb_mod)
sjPlot::plot_model(wet_forb_zanb_mod)
sjPlot::plot_model(wet_forb_zanb_mod, type = "int")


#SEVER 
#Plot of the distribution of observations
ggplot(sever, aes(x = encounters, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(year~subsite)+
  theme_bw()

#Dry and wet seem to have very different distributions; test whether they could be modelled separately
sever_subsite_mod <- glmmTMB(encounters ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = nbinom1, data = sever) #nbinom1 better fit than poisson

check_overdispersion(sever_subsite_mod)
summary(sever_subsite_mod)

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
#summary(dry_sever_zanb_mod)
sjPlot::plot_model(dry_sever_nb_mod, type = 'int')
sjPlot::plot_model(dry_sever_zanb_mod, type = 'int')

#WET - SEVER
wet_sever_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(sever, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sever_p_mod)
check_zeroinflation(wet_sever_p_mod) #small zero inflation possibility

wet_sever_zap_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                             family = truncated_poisson, data = filter(sever, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sever_zap_mod) #improved dispersion
check_zeroinflation(wet_sever_zap_mod) #A bit worse zero modelling

wet_sever_nb_mod <- glmmTMB(encounters ~ treatment*year_scale
                             + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                             family = nbinom1, data = filter(sever, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sever_nb_mod) #improved dispersion
check_zeroinflation(wet_sever_nb_mod) #A bit worse zero modelling

wet_sever_zanb_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~1,
                            family = truncated_nbinom1, data = filter(sever, subsite == 'NAKVAKWET'))

check_overdispersion(wet_sever_zanb_mod) #improved dispersion
check_zeroinflation(wet_sever_zanb_mod) #A bit worse zero modelling

AICtab(wet_sever_p_mod, wet_sever_nb_mod,
       wet_sever_zap_mod)

summary(wet_sever_nb_mod)


#----END
