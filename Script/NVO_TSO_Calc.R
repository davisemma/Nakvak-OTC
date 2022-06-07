rm(list=ls())

#GROUND TEMPS
setwd("~/")
setwd("Data/OTC Temp Effect/Ground")
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))
getwd()
setwd("Data analysis/Data/OTC Temp Effect/Air")

#AIR TEMPS
air <- read.csv("NAKVAK_DAILY_SAT_INTERP.csv")
names(air) <- c("Date", "Tair")
air <- air %>%
  mutate(Year = substr(Date, 1, 4))

DDa <- air %>%
  mutate(Above = if_else(Tair > 0, 1, 0),
         Below = if_else(Tair < 0, 1, 0)) %>%
  group_by(Year) %>%
  summarize(TDDa = sum(Tair[Above == 1]),
            FDDa = sum(Tair[Below == 1])*-1)

#Ground
ground <- tbl_with_sources
names(ground) <- c("Date", "minT", "maxT", "Tground", "Samples", "Plot")
ground <- ground[,c(1,4,6)] %>%
  mutate(Year = substr(Date, 1, 4),
         Plot = substr(Plot, 3, 20),
         Subsite = substr(Plot, 7,9),
         Treatment = substr(Plot, 3,5))

###ENSURE THAT THERE IS REASONABLE DATA COVERAGE BEFORE AVERAGING (e.g., at least one logger in each per year)

coverage <- ground %>%
  mutate(Month = substr(Date, 6,7),
         Counter = 1) %>%
  group_by(Plot, Subsite, Year) %>%
  summarize(Obs = sum(Counter))

good_obs <- coverage %>%
  filter(Obs >= 360)

good_ground <- merge(good_obs[1:3], ground, by = c('Plot', 'Year', 'Subsite'), all.y = FALSE)

DDs <- good_ground %>%
  mutate(Above = if_else(Tground > 0, 1, 0),
       Below = if_else(Tground < 0, 1, 0)) %>%
  group_by(Plot,Subsite,Year, Treatment) %>%
  summarize(TDDs = sum(Tground[Above == 1]),
            FDDs = sum(Tground[Below == 1])*-1)


DD_dat <- merge(DDs, DDa, by = "Year")

DD_calc <- DD_dat %>%
  mutate(NVO = (FDDa - FDDs)/365,
         TSO = (TDDs - TDDa)/365)

DD_plot <- melt(DD_calc, id.vars = c("Year", "Plot", "Subsite", "Treatment"), 
              measure.vars = c("NVO", "TSO"),
              variable.name = c("Variable")) %>%
  mutate(Subsite = factor(Subsite, levels = c('WET', 'DRY'))) %>%
  filter(Year != c('2011'))


#Plotting
NVO_plot <- ggplot(filter(DD_plot, Variable == 'NVO'), aes(x = Year, y = value, fill = Treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75)+
  theme_pubr(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_simpsons(alpha = 0.8)+
  labs(y = 'Nival season offset (°C)')+
  facet_wrap(~Subsite)
NVO_plot

TSO_plot <- ggplot(filter(DD_plot, Variable == 'TSO'), aes(x = Year, y = value, fill = Treatment))+
  geom_boxplot(size = .3, outlier.size = 0.75)+
  theme_pubr(base_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_simpsons(alpha = 0.8)+
  labs(y = 'Thawing season offset (°C)')+
  facet_wrap(~Subsite)

TSO_plot


ggarrange(NVO_plot + font("x.text", size = 8) + font("y.text", size = 8),
          TSO_plot + font("x.text", size = 8) + font("y.text", size = 8),
          common.legend = TRUE,
          legend = c("bottom"))
#6.5 x 3.75


##NVO DAT
nvo.dat <- filter(DD_plot, Variable == 'NVO')
nvo.dat$NVO <- nvo.dat$value
nvo.dat$YearNum <- scale(as.numeric(nvo.dat$Year), center = TRUE, scale = TRUE)

nvo.mod <- lmer(NVO ~ Subsite*Year*Treatment + (1|Plot), data = nvo.dat)
summary(nvo.mod)
plot(nvo.mod)
qqnorm(resid(nvo.mod))
qqline(resid(nvo.mod))

nvo.con <- emmeans(nvo.mod , ~Treatment|Subsite)
contrast(nvo.con, "poly", "tukey")  


##TSO DAT
tso.dat <- filter(DD_plot, Variable == 'TSO')

tso.mod <- lmer(value ~ Year*Treatment + (1|Plot), data = filter(tso.dat, Subsite == 'DRY'))
summary(tso.mod)
plot(tso.mod)
qqnorm(resid(tso.mod))
qqline(resid(tso.mod))

sjPlot::tab_model(tso.mod)
sjPlot::tab_model(nvo.mod)

tso.con <- emmeans(tso.mod , ~Treatment|Year)
contrast(tso.con, "poly", "tukey")  


#MEAN ANNUAL GST
test <- good_ground %>%
  group_by(Year, Plot) %>%
  summarize(Tave = mean(Tground, na.rm = TRUE)) %>%
  spread(., "Plot", value = Tave)

#write.csv(test, "~/Desktop/Tave_GST_Ann_Plots.csv")

#CORRELATION between plot canopy height and offsets
ave.offsets <- DD_plot %>%
  group_by(Plot, Subsite, Variable) %>%
  summarize(MeanOffset = mean(value, na.rm = TRUE))

#write.csv(ave.offsets, "~/Desktop/Offsets.csv")

sum.dat <- read_csv("Offset Plot Height/Offsets_Height.csv")
corr.test(filter(sum.dat, Variable == 'NVO')$MeanOffset, filter(sum.dat, Variable == 'NVO')$MeanHeight, method = 'spearman')
corr.test(filter(sum.dat, Variable == 'TSO')$MeanOffset, filter(sum.dat, Variable == 'TSO')$MeanHeight, method = 'spearman')

nvo.wet <- filter(sum.dat, Variable == 'NVO')# %>%
  filter(., Subsite == 'WET')

plot(nvo.wet$MeanOffset, nvo.wet$MeanHeight)






 