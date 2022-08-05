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
data <- read.csv("Point Frame/plot_data_fin.csv") 
holders <- read.csv("Point Frame/plot_year_genus_fin.csv") #file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate mean encounters for each lifeform x plot
mean_height <- data %>%
  filter(., status == "LIVE") %>%
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
         year_scale = scale(year)) #converted to z-score for modelling

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

gram_subsite_mod <- glmmTMB(mean_height ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = gaussian, data = gram) 

check_model(gram_subsite_mod) #seems to be a good fit
summary(gram_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

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

sdeci_subsite_mod <- glmmTMB(mean_height ~ subsite
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = gaussian, data = sdeci) 

check_model(sdeci_subsite_mod) #seems to be a good fit
summary(sdeci_subsite_mod)
#Strong effect of subsite, will model dry vs. wet separately from here on

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

sever_subsite_mod <- glmmTMB(mean_height ~ subsite
                             + (1|plot_pair/plot) + (1|year_scale),
                             family = gaussian, data = sever) 

check_model(sever_subsite_mod) #seems to be a good fit
summary(sever_subsite_mod)
#No subsite effect?

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

forb_subsite_mod <- glmmTMB(mean_height ~ subsite
                            + (1|plot_pair/plot) + (1|year_scale),
                            family = gaussian, data = forb) 

check_model(forb_subsite_mod) #seems to be a good fit
summary(forb_subsite_mod)

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


#Plotting for paper figures ----
wet_shrub_plot <- sjPlot::plot_model(wet_sdeci_mod,
                                      axis.labels=c("Treatment [OTC] * Year", "Year", "Treatment [OTC]"),
                                      show.values=TRUE, show.p=TRUE,
                                      title="Ave. height - Deciduous shrubs",
                                      vline.color = 'light grey',
                                      value.size = 3,
                                      size = 10,
                                      dot.size = 2,
                                      line.size = 0.5,
                                     value.offset = .3)+
  scale_color_manual(values = c("#148335", "#B765A5"))+
  plot_theme

wet_shrub_plot

wet_forb_plot <- sjPlot::plot_model(wet_forb_mod, 
                                     axis.labels=c("Treatment [OTC] * Year", "Year", "Treatment [OTC]"),
                                     show.values=TRUE, show.p=TRUE,
                                     title="Ave. height - Forbs",
                                     vline.color = 'light grey',
                                     value.size = 3,
                                     size = 10,
                                     dot.size = 2,
                                     line.size = 0.5,
                                     value.offset = .3)+
  scale_color_manual(values = c("#148335", "#B765A5"))+
  plot_theme+
  theme(axis.text.y=element_blank())

wet_forb_plot

plot_row <- cowplot::plot_grid(wet_shrub_plot, wet_forb_plot,
                               rel_widths = c(1.65,1))

plot_row

title <- ggdraw() + 
  draw_label(
    "Estimated effects of treatment (OTC) and time (scaled year) on \naverage life form height in wet plots",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 10)

plot_grid(
  title, plot_row,
  ncol = 1,
  rel_heights = c(0.09, 1)) # rel_heights values control vertical title margins






