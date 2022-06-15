#Script to test change in MAX height of lifeforms
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
data <- read.csv("Point Frame/plot_data.csv") 
holders <- read.csv("Point Frame/plot_year_genus.csv") #file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate n encounters for each lifeform x plot
max_height <- data %>%
  filter(., status == "LIVE") %>%
  mutate(height = replace_na(height, 0)) %>%
  group_by(subsite, treatment, plot, year, lifeform) %>%
  summarise(max_height = max(height)) 

#Merge with placeholder data
max_height_merge <- merge(max_height, holders, all.y = TRUE) %>%
  mutate(subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year)) #converted to z-score for modelling

#Create a dataframe for each lifeform
gram <- filter(max_height_merge, lifeform == 'GRAM')
sdeci <- filter(max_height_merge, lifeform == 'SDECI')
sever <- filter(max_height_merge, lifeform == 'SEVER')
forb <- filter(max_height_merge, lifeform == 'FORB')

#AVERAGE HEIGHT ANALYSIS ----
#GRAMINOIDS ----
#Plot of the distribution of observations
ggplot(gram, aes(x = as.factor(year), y = max_height, fill = treatment)) +
  geom_boxplot()+
  #geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(~subsite)+
  theme_bw()

gram_subsite_mod <- glmmTMB(max_height ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = gaussian, data = gram) 

check_model(gram_subsite_mod) #seems to be a good fit
summary(gram_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

#DRY - GRAM
dry_gram_mod <- glmmTMB(max_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKDRY'))
summary(dry_gram_mod)
check_model(dry_gram_mod)

#WET - GRAM
wet_gram_mod <- glmmTMB(max_height ~ treatment*year_scale
                        + (1|plot_pair/plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKWET'))
summary(wet_gram_mod)
check_model(wet_gram_mod)
sjPlot::plot_model(wet_gram_mod)
sjPlot::plot_model(wet_gram_mod, type = "int")

#SDECI ----
#Plot of the distribution of observations
ggplot(sdeci, aes(x = as.factor(year), y = max_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()

sdeci_subsite_mod <- glmmTMB(max_height ~ subsite
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = gaussian, data = sdeci) 

check_model(sdeci_subsite_mod) #seems to be a good fit
summary(sdeci_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

#DRY - SDECI
dry_sdeci_mod <- glmmTMB(max_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sdeci, subsite == 'NAKVAKDRY'))
summary(dry_sdeci_mod)
check_model(dry_sdeci_mod)
sjPlot::plot_model(dry_sdeci_mod)
sjPlot::plot_model(dry_sdeci_mod, type = "int")
#WET - SDECI
wet_sdeci_mod <- glmmTMB(max_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sdeci, subsite == 'NAKVAKWET'))
summary(wet_sdeci_mod)
check_model(wet_sdeci_mod)
sjPlot::plot_model(wet_sdeci_mod)

#SEVER ----
#Plot of the distribution of observations
ggplot(sever, aes(x = as.factor(year), y = max_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()

sever_subsite_mod <- glmmTMB(max_height ~ subsite
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = gaussian, data = sever) 

check_model(sever_subsite_mod) #seems to be a good fit
summary(sever_subsite_mod)
#No subsite effect?

#DRY - SEVER
dry_sever_mod <- glmmTMB(max_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sever, subsite == 'NAKVAKDRY'))
summary(dry_sever_mod)
check_model(dry_sever_mod)
sjPlot::plot_model(dry_sever_mod)
#WET - SEVER
wet_sever_mod <- glmmTMB(max_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(sever, subsite == 'NAKVAKWET'))
summary(wet_sever_mod)
check_model(wet_sever_mod)
sjPlot::plot_model(wet_sever_mod)

#FORB ----
#Plot of the distribution of observations
ggplot(forb, aes(x = as.factor(year), y = max_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()

forb_subsite_mod <- glmmTMB(max_height ~ subsite
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = gaussian, data = forb) 

check_model(forb_subsite_mod) #seems to be a good fit
summary(forb_subsite_mod)

#DRY - FORB
dry_forb_mod <- glmmTMB(max_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(forb, subsite == 'NAKVAKDRY'))
summary(dry_forb_mod)
check_model(dry_forb_mod)
sjPlot::plot_model(dry_forb_mod)

#WET - FORB
wet_forb_mod <- glmmTMB(max_height ~ treatment*year_scale
                         + (1|plot_pair/plot) + (1|year_scale),
                         family = gaussian, data = filter(forb, subsite == 'NAKVAKWET'))
summary(wet_forb_mod)
check_model(wet_forb_mod)
sjPlot::plot_model(wet_forb_mod)

