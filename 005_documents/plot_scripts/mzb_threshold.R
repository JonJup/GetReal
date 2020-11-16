# -------------------------- #
### --- Threshold plot --- ###
# -------------------------- #

# date: 09.11.
# GetReal
# Working Package2
# Macroinvertebrates

dir_data = "~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"

dt_plot = readRDS(file.path(dir_data, "01_data_threshold_plot.RDS"))
dt_plot$taxon_level = factor(dt_plot$taxon_level, levels = c("species", "genus", "family", "order", "subclass"))

threshplot_total = dt_plot %>% 
        ggplot(aes(x = thresholds, y = number, col = taxon_level)) + 
        geom_line(size = 1.3)  + 
        scale_y_log10() + 
        xlab(label="number of observations") + 
        xlab("theshold [%]") +
        labs(col = "taxon level") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
threshplot_percent = dt_plot %>% 
        ggplot(aes(x = thresholds, y = percent, col = taxon_level)) + 
        geom_line(size = 1.3)  + 
        xlab(label="percent of observations") + 
        xlab("theshold [%]") +
        labs(col = "taxon level") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
prow = plot_grid(
        threshplot_total   + theme(legend.position = "none"), 
        threshplot_percent + theme(legend.position = "none"),
        labels = c("A", "B"))
legend <- get_legend(
        threshplot_percent + theme(legend.position = "bottom")
)

plot_thresh = plot_grid(prow, legend, ncol = 1, rel_heights = c(1,.1))
rm(prow, legend, threshplot_percent, threshplot_total)
