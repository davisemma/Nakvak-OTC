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
  mutate(NVO = (FDDa - FDDs)/Obs, #changed from 365 to 'n Obs' because some years have 364-366 days of data
         TSO = (TDDs - TDDa)/Obs)

DD_plot <- pivot_longer(DD_calc[,c(1:4, 10:11)], cols = c("NVO", "TSO"), names_to = "Variable", values_to = "Value") %>%
  mutate(Subsite = factor(Subsite, levels = c('Wet', 'Dry'))) %>%
  filter(Year != c('2011'))

#Plotting
NVO_plot <- ggplot(filter(DD_plot, Variable == 'NVO'), aes(x = Year, y = Value, fill = Treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75, position = position_dodge(preserve = "single"))+
  theme_pubr(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #scale_fill_simpsons(alpha = 0.8)+
  scale_fill_manual(values = c("plum1", 'seagreen3'))+
  labs(y = 'Nival season offset (°C)')+
  facet_wrap(~Subsite)+
  ggtitle("NVO")
NVO_plot

TSO_plot <- ggplot(filter(DD_plot, Variable == 'TSO'), aes(x = Year, y = Value, fill = Treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75, position = position_dodge(preserve = "single"))+
  theme_pubr(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #scale_fill_simpsons(alpha = 0.8)+
  scale_fill_manual(values = c("plum1", 'seagreen3'))+
  labs(y = 'Thawing season offset (°C)')+
  facet_wrap(~Subsite)+
  ggtitle("TSO")

TSO_plot


ggarrange(NVO_plot + font("x.text", size = 8) + font("y.text", size = 8),
          TSO_plot + font("x.text", size = 8) + font("y.text", size = 8),
          ncol = 1,
          common.legend = TRUE,
          legend = c("bottom"))
#6.5 x 3.75


##NVO DAT
nvo.dat <- filter(DD_plot, Variable == 'NVO')
nvo.dat$NVO <- nvo.dat$Value
nvo.dat$YearNum <- as.numeric(scale(as.numeric(nvo.dat$Year)), center = TRUE, scale = TRUE)

nvo_dry_mod <- glmmTMB(NVO ~ Treatment * YearNum + (1|Plot), 
                   family = 'gaussian',data = filter(nvo.dat, Subsite == "Dry"))


nvo_mod <- glmmTMB(NVO ~ Subsite * Treatment + (1|Plot), 
                       family = 'gaussian',data = nvo.dat)

summary(nvo_mod)

##TSO DAT
tso.dat <- filter(DD_plot, Variable == 'TSO')
tso.dat$TSO <- tso.dat$Value
tso.dat$YearNum <- as.numeric(scale(as.numeric(tso.dat$Year)), center = TRUE, scale = TRUE)

tso_mod <- glmmTMB(TSO ~ Treatment + (1|Plot), 
                   family = 'gaussian', data = filter(tso.dat, Subsite == 'Wet'))

summary(tso_mod)
