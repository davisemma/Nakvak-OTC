library("vegan")
library("ggplot2")

#READ DATA ----
data <- read.csv("Point Frame/plot_data.csv") 
holders <- read.csv("Point Frame/plot_year_genus_fin.csv") #file with all lifeform x plot combinations

#FROMAT DATA ----
#Calculate n encounters for each lifeform x plot
encounters <- data %>% 
  filter(., status == 'LIVE') %>%
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
life_dat <- as_tibble(encounters_wide[,5:12]) %>%
  column_to_rownames(., var = "full_plot_name")

#Identify the columns that contains the descriptive/environmental data (dataframe 2)
chr_dat <- encounters_wide[,1:5]

#ordination by NMDS
NMDS <- metaMDS(life_dat)
NMDS

#########################
#Data visualisation (THIS IS AN UPDATED VERSION OF THE SCRIPT, NOW USING GGPLOT)

#Extract the axes scores
plot_dat <- (scores(NMDS)$sites) %>%
  as_tibble(rownames = "full_plot_name") %>%
  inner_join(., chr_dat, by="full_plot_name")

labels <- scores(NMDS)$species %>%
  as_tibble(rownames = "lifeform")
  

x <- ggplot(plot_dat, aes(x=NMDS1, y=NMDS2, color = subsite, fill = subsite)) +
  stat_ellipse(level = 0.5, geom = "polygon", alpha = 0.4)+
  geom_point()+
  facet_wrap(~year)
  
x
x + geom_text(aes(label = lifeform), data = labels)


#####################
#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(life_dat)
anova(betadisper(distances_data, chr_dat$subsite))

#Bootstrapping and testing for differences between the groups
fit <- adonis(data_1 ~ Habitat, data=data_2, permutations=999, method="bray")

