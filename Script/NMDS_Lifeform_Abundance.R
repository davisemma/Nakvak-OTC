library("vegan")
library("ggplot2")
library("ggsci")
library("ggpubr")
library("tidyverse")

#READ DATA ----
setwd("Data")
data <- read.csv("Point Frame/plot_data_QC_ELD.csv") 
holders <- read.csv("Point Frame/plot_year_combination.csv") %>%
  filter(., year != 2008)#file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate n encounters for each lifeform x plot
#Remove non-living encounters, anything from 2008, create bryophyte class
encounters <- data %>% 
  filter(., status == 'LIVE') %>%
  filter(., year != 2008) %>%
  mutate(lifeform = str_replace(lifeform, 'MOSS', 'BRYOPHYTE'),
         lifeform = str_replace(lifeform, 'LIVERWORT', 'BRYOPHYTE')) %>%
  group_by(subsite, treatment, plot, year, lifeform) %>%
  summarise(encounters = n()) 

#Merge encounters with placeholder file, replace 'NA' with '0', format
encounters_merge <- merge(encounters, holders, all.y = TRUE) %>%
  mutate(encounters = replace_na(encounters, 0),
         subsite = as.factor(subsite), 
         plot = as.factor(plot),
         treatment = as.factor(treatment),
         lifeform = as.factor(lifeform),
         full_plot_name = paste(subsite, plot, treatment, year, sep = "_"))

encounters_wide <- pivot_wider(encounters_merge, names_from = lifeform, values_from = encounters)

#subset the dataframe on which to base the ordination (dataframe 1)
life_dat <- filter(as_tibble(encounters_wide), treatment == 'CTL')  %>%
  column_to_rownames(., var = "full_plot_name") %>%
  dplyr::select(., c(BRYOPHYTE:SEVER))

#Identify the columns that contains the descriptive/environmental data (dataframe 2)
chr_dat <- filter(encounters_wide, treatment == 'CTL')[,1:5]

#Run ordination by NMDS
NMDS <- metaMDS(life_dat, k = 2, trymax = 100)
NMDS
stressplot(NMDS)

#Check assumptions and test for differences ----

#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(life_dat)
anova(betadisper(distances_data, chr_dat$subsite))

#Testing for differences between the groups
ano <- anosim(life_dat, chr_dat$subsite, distance = "bray", permutations = 9999)
ano

#Data visualisation ----

#Extract the axes scores
plot_dat <- (scores(NMDS)$sites) %>%
  as_tibble(rownames = "full_plot_name") %>%
  inner_join(., chr_dat, by="full_plot_name")

labels <- scores(NMDS)$species %>%
  as_tibble(rownames = "lifeform") %>%
  mutate(labels = c("BRYOPHYTE", "FORB", "GRAM", "LICHEN", "SDECI", "SEVER"))

plot_theme <-   theme_pubr() + 
  theme(legend.position = "top",
    legend.justification = c(0,-1),
    legend.box.margin = margin(t = -3, b = -10, l = -22, unit = "pt"),
                      legend.key.size = unit(1, 'lines'),
                      legend.text = element_text(size = 8),
                      legend.title = element_text(size = 8, ),
                      plot.title = element_text(size = 10, vjust = 2, face = 'bold', margin = margin(t = 3, unit = 'pt')),
                      plot.title.position = "plot",
                      axis.title.x = element_text(size = 8,),
                      axis.text.x = element_text(size = 8),
                      axis.title.y = element_text(size = 8,),
                      axis.text.y = element_text(size = 8),
                      axis.line = element_line(colour = 'black', size = 0.3),)

nmds_plot <- ggplot()+
  ggtitle("nMDS of life form abundance in CTL plots") +
  stat_ellipse(data = plot_dat, aes(x=NMDS1, y=NMDS2, color = subsite, fill = subsite), level = 0.75, geom = "polygon", show.legend=FALSE)+
  geom_point(data = plot_dat, aes(x=NMDS1, y=NMDS2, fill = subsite, shape = as.factor(year)), color = 'black', show.legend = TRUE)+
  scale_color_manual(values = c('#EEE191','#82AAD9'),
                     labels = c("Dry", "Wet"),
                     name = 'Plot moisture class')+
  scale_fill_manual(values = c('#EEE191','#82AAD9'),
                    labels = c("Dry", "Wet"),
                    name = 'Plot moisture class')+
  scale_shape_manual(values = c(21, 22, 23),
                     name = 'Year')+
  geom_text(data=labels, aes(x = NMDS1, y = NMDS2, label = labels), 
            hjust = 0,  vjust = 0, size = 2)+
  plot_theme+
  xlab('nMDS 1')+
  ylab('nMDS 2')
  
nmds_plot

#Export size: 4 x 4 
ggsave("~/Desktop/nmds_plot.tiff", width = 3.5, height = 4, units = 'in', dpi = 400)
ggsave("~/Desktop/nmds_plot.pdf", width = 4, height = 3.5, units = 'in')




