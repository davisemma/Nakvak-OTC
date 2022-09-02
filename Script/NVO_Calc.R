rm(list=ls())

#GROUND TEMPS
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(path = "OTC Temp Effect/Ground Daily",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))


#AIR TEMPS
air <- read.csv("OTC Temp Effect/Air/NAK_1981_2022_daily_air.csv")
names(air) <- c("Date", "Tair")
air <- air %>%
  mutate(Year = substr(Date, 1, 4)) %>%
  filter(., Year < 2022)

DDa <- air %>%
  mutate(Above = if_else(Tair > 0, 1, 0),
         Below = if_else(Tair < 0, 1, 0)) %>%
  group_by(Year) %>%
  summarize(TDDa = sum(Tair[Above == 1]),
            FDDa = sum(Tair[Below == 1])*-1)

#Ground
ground <-tbl_with_sources
names(ground) <- c("Date","Tground", "Plot")
ground <- ground %>%
  mutate(Year = substr(Date, 1, 4),
         Plot = substr(Plot, 30,41),
         Subsite = substr(Plot, 10,12),
         Treatment = substr(Plot, 6,8))

###ENSURE THAT THERE IS REASONABLE DATA COVERAGE BEFORE AVERAGING (e.g., at least one logger in each per year)

coverage <- ground %>%
  mutate(Month = substr(Date, 6,7),
         Counter = 1) %>%
  filter(.,!is.na(Tground)) %>%
  group_by(Plot, Subsite, Year) %>%
  summarize(Obs = sum(Counter))

good_obs <- coverage %>%
  filter(Obs >= 360)

good_ground <- merge(good_obs[1:4], ground, by = c('Plot', 'Year', 'Subsite'), all.y = FALSE)

DDs <- good_ground %>%
  mutate(Above = if_else(Tground > 0, 1, 0),
       Below = if_else(Tground < 0, 1, 0)) %>%
  group_by(Plot,Subsite,Year, Treatment, Obs) %>%
  summarize(TDDs = sum(Tground[Above == 1], na.rm = TRUE),
            FDDs = sum(Tground[Below == 1], na.rm = TRUE)*-1)

DD_dat <- merge(DDs, DDa, by = "Year")

DD_calc <- DD_dat %>%
  mutate(NVO = (FDDa - FDDs)/Obs,
         TSO = (TDDs - TDDa)/Obs)

DD_plot <- pivot_longer(DD_calc[,c(1:4, 10:11)], cols = c("NVO", "TSO"), names_to = "Variable", values_to = "Value") %>%
  mutate(Subsite = factor(Subsite, levels = c('Dry', 'Wet'))) %>%
  filter(Year != c('2011'))

#Plotting
facet_labels <- as_labeller(c('Dry' = 'Dry plots', 'Wet' = 'Wet plots'))
facet_labels_blank <- as_labeller(c('Dry' = '', 'Wet' = ''))

plot_theme <-   theme_few() + 
  theme(axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(colour = 'black', size = 0),
        strip.text.x = element_text(size = 8, hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        panel.border = element_rect(size = .3),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),)

NVO_plot <- ggplot(filter(DD_plot, Variable == 'NVO'), aes(x = as.numeric(Year), y = Value, group = interaction(Treatment, Year), fill = interaction(Treatment, Subsite)))+
  geom_boxplot(size = .2, outlier.size = 0.5, position = position_dodge2(preserve = "single", padding = 0))+
  scale_x_continuous(breaks=seq(2012, 2020, 1),
                     labels = c(2012, '', 2014, '', 2016, '', 2018, '', 2020))+
  scale_fill_manual(values = c("#E7D580", '#FFF7D4', '#B3C8E3', '#E5EDF8'))+
  labs(y = 'Nival offset',
       x = '')+
  facet_wrap(~Subsite, labeller = facet_labels)+
  plot_theme+
  theme(plot.margin = margin(2, 6, 0, 6))+
  theme(legend.position= "none")

NVO_plot

fake <- ggplot(filter(DD_plot, Variable == 'NVO'), aes(x = as.numeric(Year), y = Value, group = Treatment, fill = Treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75, position = position_dodge2(preserve = "single", padding = 0))+
  scale_x_continuous(breaks=seq(2012, 2020, 1),
                     labels = c(2012, '', 2014, '', 2016, '', 2018, '', 2020))+
  scale_fill_manual(values = c("#BDBEBF", '#E8E9EB'), name = 'Treatment',
                    labels = c('Control', 'OTC'))+
  labs(y = '# Snow days',
       x = '')+
  facet_wrap(~Subsite, labeller = facet_labels)+
  plot_theme+
  theme(plot.margin = margin(2, 6, -10, 6))

legend <- cowplot::get_legend(fake + theme(legend.position = 'top',
                                  legend.box.margin = margin(0, 0, 0, 0),
                                  legend.text = element_text(size = 8),
                                  legend.title = element_text(size = 8),
                                  legend.justification = "left"))

cowplot::plot_grid(legend, NVO_plot, ncol = 1, rel_heights = c(0.15, 1),
           vjust = 0.5, hjust = -1)



##NVO DAT
nvo.dat <- filter(DD_plot, Variable == 'NVO')
nvo.dat$NVO <- nvo.dat$Value
nvo.dat$YearNum <- as.numeric(scale(as.numeric(nvo.dat$Year)), center = TRUE, scale = TRUE)
library(glmmTMB)
nvo_dry_mod <- glmmTMB(NVO ~ Treatment * YearNum + (1|Plot), 
                   family = 'gaussian',data = filter(nvo.dat, Subsite == "Dry"))


nvo_mod <- glmmTMB(NVO ~ Subsite * Treatment + (1|Plot), 
                       family = 'gaussian',data = nvo.dat)

summary(nvo_mod)
summary(nvo_dry_mod)

