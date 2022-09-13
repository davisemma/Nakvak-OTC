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

holders_ground <- filter(holders, lifeform == 'FORB' | lifeform == 'GRAM') %>%
  mutate(lifeform = if_else(lifeform == 'FORB', 'LITTER', 'NON'))

#FROMAT DATA ----
#Calculate n encounters for each of 6 lifeforms per plot
encounters <- data %>% 
  filter(., lifeform == 'LITTER' | lifeform == 'BARE' | lifeform == 'SOIL') %>% #select only 'live' encounters
  filter(., year != 2008) %>% #remove 2008 observations
  mutate(lifeform = if_else(lifeform == 'BARE', 'NON', if_else(lifeform == 'SOIL', 'NON', lifeform)))%>%
  group_by(subsite, treatment, plot, year, lifeform) %>% 
  summarise(encounters = n()) #number of encounters of each lifeform in each plot

#Merge encounters with placeholder file, replace 'NA' with '0', format
encounters_merge <- merge(encounters, holders_ground, all.y = TRUE) %>% #merge w 'holders' so that all combinations are represented (i.e., '0's are meaningful and should be included)
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
bare <- filter(encounters_merge, lifeform == 'NON')
litter <- filter(encounters_merge, lifeform == 'LITTER')

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

#WET PLOT LITTER
wet_bare_p_mod <- glmmTMB(encounters ~ treatment*year_scale
                            + (1|plot_pair/plot) + (1|year_scale), ziformula = ~0,
                            family = poisson, data = filter(bare, subsite == 'NAKVAKWET'))

check_overdispersion(wet_bare_p_mod)
check_model(wet_bare_p_mod)
summary(wet_bare_p_mod)
plot_model(wet_bare_p_mod, type = 'int')





