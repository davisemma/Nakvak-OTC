plot(ggeffects::ggpredict(dry_lichen_nb_mod, terms = c("treatment", "year_scale")))

plot(ggeffects::ggpredict(wet_sever_nb_mod, terms = c("treatment", "year_scale")))


ggeffects::ggpredict(dry_lichen_nb_mod, terms = c("treatment"))

sjPlot::plot_models(dry_lichen_nb_mod, wet_sever_nb_mod,
                    axis.labels=c("Treatment [OTC] * Year", "Year", "Treatment [OTC]"),
                    show.values=TRUE, show.p=TRUE,
                    title="Moss",
                    vline.color = 'light grey',
                    value.size = 3,
                    dot.size = 1,
                    line.size = 0.5)+
  scale_color_manual(values = c("#148335", "#B765A5"))+
  scale_shape_manual(values = c(5))+
  plot_theme
