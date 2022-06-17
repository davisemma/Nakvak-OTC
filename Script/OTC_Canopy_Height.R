#Script to test change in canopy height
#of the various plots/lifeforms

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
data <- read.csv("Point Frame/plot_data.csv") 
holders <- read.csv("Point Frame/plot_year_genus.csv") #file with all lifeform x plot combinations

#Filter data for living material, hit order == 1 (top-most encounter)
data_subset <- filter(data, status == "LIVE") %>%
  group_by(plot, year, x, y) %>%
  filter(., hit_order == min(hit_order))



data_subset <- filter(data, hit_order == 1)







