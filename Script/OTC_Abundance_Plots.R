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

dry <- sjPlot::plot_models(dry_gram_p_mod,
                           dry_forb_p_mod,
                           dry_sdeci_p_mod,
                           dry_sever_nb_mod,
                           dry_lichen_nb_mod,
                           dry_moss_nb_mod,
                    title="Dry plots",
                    axis.labels=c("Treatment [OTC]\n* Year", "Year", "Treatment\n[OTC]"),
                    p.shape = TRUE,
                    p.threshold = c(0.05),
                    show.values=FALSE, show.p=TRUE,
                    show.legend = FALSE,
                    vline.color = '#F0F1F3',
                    value.size = 3,
                    dot.size = 2,
                    line.size = 0.5,
                    spacing = 0.65,
                    axis.lim = c(0.1, 10))+
  scale_color_manual(values = c('#85D1F0', '#B765A5', '#A4A4A6', '#5D8A55', '#EF9FEF', '#ECD982'))+
  plot_theme+
  theme(plot.margin = margin(3, 6, 3, 6))+
  scale_shape_manual(values = c(1, 19))

wet <- sjPlot::plot_models(wet_gram_nb_mod,
                           wet_forb_nb_mod,
                           wet_sdeci_nb_mod,
                           wet_sever_p_mod,
                           wet_lichen_p_mod,
                           wet_moss_nb_mod,
                    title="Wet plots",
                    axis.labels=c("", "", ""),
                    p.shape = TRUE,
                    p.threshold = c(0.05),
                    show.values=FALSE, show.p=TRUE,
                    show.legend = FALSE,
                    vline.color = '#F0F1F3',
                    value.size = 3,
                    dot.size = 2,
                    line.size = 0.5,
                    spacing = 0.65,
                    axis.lim = c(0.1, 10))+
  scale_color_manual(values = c('#85D1F0', '#B765A5', '#A4A4A6', '#5D8A55', '#EF9FEF', '#ECD982'))+
  plot_theme+
  theme(plot.margin = margin(3, 6, 3, 6))+
  scale_shape_manual(values = c(1, 19))
  

fake <- sjPlot::plot_models(dry_gram_p_mod,
                            dry_forb_p_mod,
                            dry_sdeci_p_mod,
                            dry_sever_nb_mod,
                            dry_lichen_nb_mod,
                            dry_moss_nb_mod,
                           title="fakers",
                           axis.labels=c("Treatment [OTC]\n* Year", "Year", "Treatment\n[OTC]"),
                           vline.color = 'light grey',
                           value.size = 3,
                           dot.size = 2,
                           line.size = 0.5,
                           spacing = 0.65,
                           legend.title = 'Lifeform',
                           #p.shape = TRUE,
                           m.labels = c('Graminoid', 'Forb', 'Deciduous shrub', 'Evergreen shrub', 'Lichen', 'Moss'),
                           p.threshold = c(0.05))+
  scale_color_manual(values = c('#85D1F0', '#B765A5', '#A4A4A6', '#5D8A55', '#EF9FEF', '#ECD982'))+
  plot_theme+
  theme(plot.margin = margin(6, 0, 6, 0))+
  scale_shape_manual(values = c(1, 19))


data <- cowplot::plot_grid(dry, wet, ncol = 2, rel_widths = c(1.35, 1))
legend <- get_legend(fake + theme(legend.position = 'top',
                                  legend.box.margin = margin(0, 0, 0, 0),
                                  legend.text = element_text(size = 8),
                                  legend.title = element_text(size = 8),
                                  legend.justification = "left"))
cowplot::plot_grid(legend, data, ncol = 1, rel_heights = c(0.2, 1))


# #DRY MODELS
# dry_gram_p_mod,
# dry_forb_p_mod,
# dry_sdeci_p_mod,
# dry_sever_nb_mod,
# dry_lichen_nb_mod,
# dry_moss_nb_mod,
# 
# #WET MODELS
# wet_gram_nb_mod,
# wet_forb_nb_mod,
# wet_sdeci_nb_mod,
# wet_sever_p_mod,
# wet_lichen_p_mod,
# wet_moss_nb_mod






  
