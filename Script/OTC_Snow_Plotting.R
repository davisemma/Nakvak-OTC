#Plotting theme ---
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


snow_days <- ggplot(snow_day_complete, aes(x = (year), y = n_snow_days, group = interaction(treatment, year), fill = interaction(treatment, subsite)))+
  geom_boxplot(size = .2, outlier.size = 0.5, position = position_dodge2(preserve = "single", padding = 0))+
  scale_x_continuous(breaks=seq(2012, 2020, 1),
                     labels = c(2012, '', 2014, '', 2016, '', 2018, '', 2020))+
  scale_fill_manual(values = c("#E7D580", '#FFF7D4', '#B3C8E3', '#E5EDF8'), name = 'Treatment',
                    labels = c('Dry - control', 'Dry - OTC', 'Wet - control', 'Wet - OTC'))+
  labs(y = '# Snow days',
       x = '')+
  facet_wrap(~subsite, labeller = facet_labels)+
  plot_theme+
  theme(plot.margin = margin(2, 6, -10, 6))+
  theme(legend.position= "none")

snow_duration <- ggplot(snow_duration, aes(x = (year), y = snow_free , group = interaction(treatment, year), fill = interaction(treatment, subsite)))+
  geom_boxplot(size = .2, outlier.size = 0.5, position = position_dodge2(preserve = "single", padding = 0))+
  scale_fill_manual(values = c("#E7D580", '#FFF7D4', '#B3C8E3', '#E5EDF8'), name = 'Treatment')+
  labs(y = 'Snow-free period (# days)',
       x = '')+
  facet_wrap(~subsite, labeller = facet_labels_blank)+
  scale_x_continuous(breaks=seq(2012, 2020, 1),
                     labels = c(2012, '', 2014, '', 2016, '', 2018, '', 2020))+
  ylim(60, 170)+
  plot_theme+
  theme(legend.position= "none")+
  theme(plot.margin = margin(0, 6, 0, 6))

fake <- ggplot(snow_day_complete, aes(x = (year), y = n_snow_days, group = interaction(treatment, year), fill = (treatment)))+
  geom_boxplot(size = .3, outlier.size = 0.75, position = position_dodge2(preserve = "single", padding = 0))+
  scale_x_continuous(breaks=seq(2012, 2020, 1),
                     labels = c(2012, '', 2014, '', 2016, '', 2018, '', 2020))+
  scale_fill_manual(values = c("#BDBEBF", '#E8E9EB'), name = 'Treatment',
                    labels = c('Control', 'OTC'))+
  labs(y = '# Snow days',
       x = '')+
  facet_wrap(~subsite, labeller = facet_labels)+
  plot_theme+
  theme(plot.margin = margin(2, 6, -10, 6))


legend <- get_legend(fake + theme(legend.position = 'top',
                               legend.box.margin = margin(0, 0, 3, 0),
                               legend.text = element_text(size = 8),
                               legend.title = element_text(size = 8),
                               legend.justification = "left"))

plot_grid(legend, snow_days, snow_duration, ncol = 1, rel_heights = c(0.15, 1, 1),
          labels = c('', 'A', 'B'), vjust = 0.7, hjust = -1, label_size = 10)

#Export - 4.5 x 5 
