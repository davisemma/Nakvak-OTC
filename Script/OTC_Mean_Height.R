#Script to test change in mean height of lifeforms
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
data <- read.csv("plot_data.csv") 
holders <- read.csv("plot_year_genus.csv") #file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate n encounters for each lifeform x plot
ave_height <- data %>%
  filter(., status == "LIVE") %>%
  mutate(height = replace_na(height, 0)) %>%
  group_by(subsite, treatment, plot, year, lifeform) %>%
  summarise(ave_height = mean(height)) 

#Merge with placeholder data
ave_height_merge <- merge(ave_height, holders, all.y = TRUE) %>%
  mutate(subsite = as.factor(subsite), 
         plot_pair = substr(plot,1,nchar(plot)-1),
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         year_scale = scale(year)) #converted to z-score for modelling

#Create a dataframe for each lifeform
gram <- filter(ave_height_merge, lifeform == 'GRAM')
sdeci <- filter(ave_height_merge, lifeform == 'SDECI')
sever <- filter(ave_height_merge, lifeform == 'SEVER')
lichen <- filter(ave_height_merge, lifeform == 'LICHEN')
forb <- filter(ave_height_merge, lifeform == 'FORB')
moss <- filter(ave_height_merge, lifeform == 'MOSS')
liverwort <- filter(ave_height_merge, lifeform == 'LIVERWORT')

#AVERAGE HEIGHT ANALYSIS ----
#GRAMINOIDS ----
#Plot of the distribution of observations
ggplot(gram, aes(x = as.factor(year), y = ave_height, fill = treatment)) +
  geom_boxplot()+
  #geom_histogram(position = "identity", alpha = 0.7, binwidth = 1)+
  facet_grid(~subsite)+
  theme_bw()


gram_subsite_mod <- glmmTMB(ave_height ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = gaussian, data = gram) 

check_model(gram_subsite_mod) #seems to be a good fit
summary(gram_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

#DRY - GRAM
dry_gram_mod <- glmmTMB(ave_height ~ treatment*year_scale
                          + (1|plot) + (1|year_scale),
                          family = gaussian, data = filter(gram, subsite == 'NAKVAKDRY'))
summary(dry_gram_mod)
check_model(dry_gram_mod)

#WET - GRAM
wet_gram_mod <- glmmTMB(ave_height ~ treatment*year_scale
                        + (1|plot) + (1|year_scale),
                        family = gaussian, data = filter(gram, subsite == 'NAKVAKDRY'))
summary(wet_gram_mod)
check_model(wet_gram_mod)

#SDECI ----
#Plot of the distribution of observations
ggplot(sdeci, aes(x = as.factor(year), y = ave_height, fill = treatment)) +
  geom_boxplot()+
  facet_grid(~subsite)+
  theme_bw()


sdeci_subsite_mod <- glmmTMB(ave_height ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = gaussian, data = sdeci) 

check_model(sdeci_subsite_mod) #seems to be a good fit
summary(sdeci_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

#DRY - GRAM
dry_sdeci_mod <- glmmTMB(ave_height ~ treatment*year_scale
                        + (1|plot) + (1|year_scale),
                        family = gaussian, data = filter(sdeci, subsite == 'NAKVAKDRY'))
summary(dry_sdeci_mod)
check_model(dry_sdeci_mod)

#WET - GRAM
wet_sdeci_mod <- glmmTMB(ave_height ~ treatment*year_scale
                        + (1|plot) + (1|year_scale),
                        family = gaussian, data = filter(sdeci, subsite == 'NAKVAKDRY'))
summary(wet_sdeci_mod)
check_model(wet_sdeci_mod)



