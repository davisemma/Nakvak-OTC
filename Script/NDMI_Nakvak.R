######################################
# Code to identify changes in NDWI   #
# between two time periods --------- #
# LAST UPDATED: Sep 12, 2022        #
######################################
#Package: rgdal, raster, plotKML, sf
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(ggplot2)
library(ggthemes)
#Remove old files,set working directory, and import NDVI Composites
rm(list=ls())
setwd("NDMI/NDMI Composites")
stacked_files <- stack(list.files(pattern = ".tif")) 
bricked_files <- brick(stacked_files)
nakvak <- st_read("~/Documents/GitHub/Nakvak-OTC/Nakvak-OTC/Data/NDMI/Nakvak Site Layers/Nakvak_Sites_UTM20.shp")

names(bricked_files) #See that all files you want are there 

#Extracting raster values that correspond to NDMI landsat pixels ----
###
nakvak_NDMI_extract <- as_tibble(cbind(raster::extract(bricked_files, nakvak),"plot" = nakvak$Plot, "subsite" = nakvak$Subsite))
nakvak_NDMI_long <- pivot_longer(nakvak_NDMI_extract, NDMI_Composite_NAKVAK1985:NDMI_Composite_NAKVAK2021) %>%
  mutate(year = as.numeric(substr(name, 22,25)),
         value = as.numeric(value))

plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8,),
        legend.background = element_blank(),
        plot.title = element_text(size = 8, vjust = 2, margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "panel",
        strip.text.x = element_text(size = 8, face = 'bold', hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),
        axis.line = element_line(colour = "#403F3F", size = 0.3),
        panel.border  = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)

ndmi_plot <- ggplot(nakvak_NDMI_long, aes(x = year, y = value, fill = subsite, group = subsite))+
  geom_rect(
    fill = "#F2F2F2", color = NA,
    xmin = 2008,
    xmax = 2021,
    ymin = -Inf,
    ymax = Inf)+
  geom_point(shape = 21, size = 1.5)+
  geom_smooth(method = 'loess', size = 0.7, aes(color = subsite), show.legend = FALSE)+
  scale_fill_manual(values = c('#EEE191','#82AAD9'),
                    labels = c("Dry", "Wet"),
                    name = 'Plot moisture class')+
  scale_color_manual(values = c('#EEE191','#82AAD9'),
                     labels = c("Dry", "Wet"),
                     name = 'Plot moisture class')+
  ylab("July-August NDMI")+
  xlab("Year")+
  theme(plot.margin = unit(c(0.25,0.2,0.2,0.3), "cm"))+
  xlim(1980, 2020)+
  plot_theme

ndmi_plot


#CROPPING TO AREA OF INTEREST 
aoi <- raster(xmn= 479320.004, xmx= 479928.583, ymn= 6499420.978 ,ymx= 6500172.016, nrow=100,ncol=100, crs = crs(bricked_files)) 
aoi[] <-1
aoi.poly <- rasterToPolygons(aoi)


plot(bricked_files$NDMI_Composite_NAKVAK1985)
plot(aoi,add=TRUE)

bricked_crop <- crop(x = bricked_files, y = aoi, snap = 'near' )

#Manually selecting suitable rasters to use in calculation 
#based on having sufficient coverage
plot(bricked_crop[[1:10,]])
plot(bricked_crop[[11:20,]])
plot(bricked_crop[[21:30,]])
plot(bricked_crop[[31:34,]])
plot(bricked_crop[[25:34,]])
plot(bricked_crop[[35:36,]])

#Calculating timeseries mean
#Extracting suitable rasters
suit <- bricked_crop[[c(1, 3, 5, 6, 9,
                        11, 13, 14, 15, 16, 18, 19,
                        21, 22, 23, 28, 29, 30,
                        31, 32, 33),]]
names(suit)
plot(suit)
years <- as.integer(c(1985, 1987, 1989, 1990, 1993,
           1995, 1997, 1998, 1999, 2000, 2002, 2003,
           2005, 2006, 2007, 2013, 2014, 2015, 
           2016, 2017, 2018))

#Calculating mean for each year 
mean.tmp <- as.data.frame(cellStats(suit, stat='mean', na.rm=TRUE))
colnames(mean.tmp) <- c("Mean.NDMI")
mean.ndmi <- mean.tmp %>%
  mutate(Composite = row.names(.)) %>%
  mutate(Year = substr(Composite, 22, 25),
         Site = 'Nakvak')

plot_theme <-   theme_few() + 
  theme(legend.position = "top",
        legend.justification = c(0,-1),
        legend.box.margin = margin(t = -5, b = -15, l = -6, unit = "pt"),
        legend.key.size = unit(1, 'lines'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8,),
        plot.title = element_text(size = 8, vjust = 2, margin = margin(t = 3, unit = 'pt')),
        plot.title.position = "panel",
        strip.text.x = element_text(size = 8, face = 'bold', hjust = 0, margin = margin(t = 4, b = 4, l = 0, unit = 'pt')),
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_line(size = 0.3, ),
        axis.ticks.length = unit(1.5, "pt"),
        axis.line = element_line(colour = "#403F3F", size = 0.3),
        panel.border  = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)

ggplot(mean.ndmi, aes(x=as.numeric(Year), y = Mean.NDMI))+
  geom_smooth(method = 'loess', size = 0.5, alpha = 0.5, color = "#85D1F0", fill = 'light grey')+
  geom_point(shape = 21, fill = "#85D1F0", size = 2)+
  ylab("Mean July-August NDMI")+
  xlab('Year')+
  plot_theme


#write.csv(mean.ndmi, '~/Desktop/MeanNDMI_Nak_Small.csv')


##Theil-Sen analysis ---- 
require(raster)
require(rkt)
library(devtools)
install_github("leandroroser/EcoGenetics-devel")
library(EcoGenetics)
require(EcoGenetics)

# Set year range for analysis
years.ts <- as.integer(c(1985, 1986, 1987, 1988, 1989, 
                      1990, 1991, 1992, 1993, 1994,
                      1995, 1996, 1997, 1998, 1999, 
                      2000, 2001, 2002, 2003, 2004, 
                      2005, 2006, 2007, 2008, 2009,
                      2010, 2011, 2013, 2014, #No data 2012
                      2015, 2016, 2017, 2018, 2019,
                      2020, 2021))

#Run Theil-Sen calculation - uncomment to run analysis
setwd("Output")
ts.rasters <- eco.theilsen(bricked_crop, years.ts)

ts.slope <- raster("slope.tif")
ts.pval <- (ts.rasters$pvalue)

ts.pval[ts.pval > 0.05] <- NA #Areas w p > 0.05 are given NA value

ts.sig <- mask(ts.slope, ts.pval) #Mask insignificant values from slope raster

plot(ts.sig)
plot(ts.slope)

ts.pval.mask <- ts.pval
ts.pval.mask[ts.pval.mask > 0.05] <- 100 #Areas w p > 0.05 are given NA value

plot(ts.slope)

writeRaster(ts.slope, "NDMI_TheilSen_Slope.tiff",
            format="GTiff",
            overwrite=TRUE,
            NAflag=-9999)

writeRaster(ts.pval, "NDMI_TheilSen_Pval.tiff",
            format="GTiff",
            overwrite=TRUE,
            NAflag=-9999)

writeRaster(ts.pval.mask, "NDMI_TheilSen_Mask.tiff",
            format="GTiff",
            overwrite=TRUE,
            NAflag=-9999)

writeRaster(ts.sig, "NDMI_TheilSen_Sig.tiff",
            format="GTiff",
            overwrite=TRUE,
            NAflag=-9999)


