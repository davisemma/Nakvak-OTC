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

dry_height <- sjPlot::plot_models(dry_gram_mod,
                           dry_forb_mod,
                           dry_sdeci_mod,
                    dry_sever_mod,
                    dry_canopy_mod,
                    #title="Dry plots",
                    
                    axis.labels=c("Treatment [OTC]\n* Year", "Year", "Treatment\n[OTC]"),
                    p.shape = TRUE,
                    p.threshold = c(0.05),
                    show.values=FALSE, show.p=TRUE,
                    show.legend = FALSE,
                    vline.color = '#F0F1F3',
                    value.size = 3,
                    dot.size = 2.5,
                    line.size = 0.3,
                    spacing = 0.70,
                    axis.lim = c(-2, 2))+
  scale_color_manual(values = c('#609495', '#BACCBE', '#E7C37A', '#F4B7A5', '#D6865A'))+
  plot_theme+
  theme(plot.margin = margin(3, 6, 3, 6))+
  scale_shape_manual(values = c(1, 19))

wet_height <- sjPlot::plot_models(wet_gram_mod,
                           wet_forb_mod,       
                    wet_sdeci_mod,
                    wet_sever_mod,
                    wet_canopy_mod,
                    #title="Wet plots",
                    axis.labels=c("", "", ""),
                    p.shape = TRUE,
                    p.threshold = c(0.05),
                    show.values=FALSE, show.p=TRUE,
                    show.legend = FALSE,
                    vline.color = '#F0F1F3',
                    value.size = 3,
                    dot.size = 2.5,
                    line.size = 0.3,
                    spacing = 0.70)+
  scale_color_manual(values = c("#403F3F", '#A4A4A6', '#5D8A55', '#EF9FEF', '#ECD982', 'hot pink'))+
  scale_color_manual(values = c('#609495', '#BACCBE', '#E7C37A', '#F4B7A5', '#D6865A'))+
  plot_theme+
  theme(plot.margin = margin(3, 6, 3, 6))+
  scale_shape_manual(values = c(1, 19))
wet_height
  

fake_height <- sjPlot::plot_models(wet_gram_mod,
                            wet_forb_mod,       
                            wet_sdeci_mod,
                            wet_sever_mod,
                            wet_canopy_mod,
                           #title="fakers",
                           axis.labels=c("Treatment [OTC]\n* Year", "Year", "Treatment\n[OTC]"),
                           vline.color = 'light grey',
                           value.size = 3,
                           dot.size = 2,
                           line.size = 0.5,
                           spacing = 0.65,
                           legend.title = 'Lifeform',
                           p.shape = TRUE,
                           p.threshold = c(0.05),
                           m.labels = c('Graminoid', 'Forb', 'Deciduous shrub', 'Evergreen shrub', 'Max. canopy height'))+
  scale_color_manual(values = c('#609495', '#BACCBE', '#E7C37A', '#F4B7A5', '#D6865A'))+
  plot_theme+
  theme(plot.margin = margin(6, 0, 6, 0))+
  scale_shape_manual(values = c(1, 19))

data_height <- cowplot::plot_grid(dry_height, wet_height, ncol = 2, rel_widths = c(1.35, 1))
legend_height <- get_legend(fake_height + theme(legend.position = 'top',
                                  legend.box.margin = margin(0, 0, 0, 0),
                                  legend.text = element_text(size = 8),
                                  legend.title = element_text(size = 8),
                                  legend.justification = "left"))
cowplot::plot_grid(legend_height, data_height, ncol = 1, rel_heights = c(0.1, 1))

cowplot::plot_grid(data, data_height, ncol = 1, rel_heights = c(1, .8),
                   labels = c('A', 'B'),
                   label_size = 8)
#4.5 x 6.75


