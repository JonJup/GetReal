# -------------------------------------- #
### --- Sensitivity Analysis Plots --- ### 
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

# setup -------------------------------------------------------------------
if(!require(pacman))install.packages(pacman)
p_load(cowplot, 
       dplyr,
       ggplot2,
       ggrepel,
       viridis)

dir_mzb = "~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data"

# load data ---------------------------------------------------------------
dt_mzb_a = readRDS(file.path(dir_mzb, "07_2020-11-06_sensitivity_parameter_a_50steps.RDS"))
dt_mzb_b = readRDS(file.path(dir_mzb, "07_2020-11-06_sensitivity_parameter_b_50steps.RDS"))

# create plots ------------------------------------------------------------
## richness a
gg_sa_rich_mzb_a = 
        ggplot(dt_mzb_a, aes(x = threshold, y = n_all)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("Macroinvertebrate  richness") + 
        scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("a threshold") + 
        labs(col = "a Threshold") + 
        theme(legend.position = "bottom")
gg_sa_rich_mzb_a_spe = 
        ggplot(dt_mzb_a, aes(x = threshold, y = n_spe)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("species richness"  ) + 
        scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("a threshold") + 
        labs(col = "a Threshold")
gg_sa_rich_mzb_a_gen = 
        ggplot(dt_mzb_a, aes(x = threshold, y = n_gen)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("genus richness"  ) + 
        scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("a threshold") + 
        labs(col = "a Threshold")
gg_sa_rich_mzb_a_fol = 
        ggplot(dt_mzb_a, aes(x = threshold, y = n_fol)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("family richness"  ) + 
        scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("a threshold") + 
        labs(col = "a Threshold")
## richness b
gg_sa_rich_mzb_b = 
        ggplot(dt_mzb_b, aes(x = threshold, y = n_all)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("Macroinvertebrates richness") + 
        viridis::scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("b threshold") + 
        theme(legend.position = "bottom")
gg_sa_rich_mzb_b_spe = 
        ggplot(dt_mzb_b, aes(x = threshold, y = n_spe)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        # geom_text(data=dt_mzb_b %>% group_by(river_type) %>% arrange(threshold) %>% slice(1),
        #           aes(x = threshold - 0.05, label=river_type), hjust=0) + 
        # guides(colour=FALSE) +
        ggtitle("species: richness"  ) + 
        viridis::scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("b threshold") + 
        labs(col = "b Threshold")
gg_sa_rich_mzb_b_gen = 
        ggplot(dt_mzb_b, aes(x = threshold, y = n_all)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        geom_text(data=dt_mzb_b %>% group_by(river_type) %>% arrange(threshold) %>% slice(1),
                  aes(x = threshold - 0.05, label=river_type), hjust=0) + 
        guides(colour=FALSE) +
        ggtitle("genera: richness"  ) + 
        viridis::scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("b threshold") + 
        labs(col = "b Threshold")
gg_sa_rich_mzb_b_fol = 
        ggplot(dt_mzb_b, aes(x = threshold, y = n_all)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        geom_text(data=dt_mzb_b %>% group_by(river_type) %>% arrange(threshold) %>% slice(1),
                  aes(x = threshold - 0.05, label=river_type), hjust=0) + 
        guides(colour=FALSE) +
        ggtitle("families: richness"  ) + 
        viridis::scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("b threshold") + 
        labs(col = "b Threshold")

## uniqueness a 
gg_sa_uniq_mzb_a = 
        ggplot(dt_mzb_a, aes(x = threshold, y = u_all)) + 
        #geom_hline(yintercept = 1/length(unique(dt_mzb_a$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("all levels: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("a threshold") + 
        labs(col = "river type")
gg_sa_uniq_mzb_a_spe = 
        ggplot(dt_mzb_a, aes(x = threshold, y = u_all)) + 
        #geom_hline(yintercept = 1/length(unique(dt_mzb_a$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("species: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("a threshold") + 
        labs(col = "river type")
gg_sa_uniq_mzb_a_gen = 
        ggplot(dt_mzb_a, aes(x = threshold, y = u_all)) + 
        #geom_hline(yintercept = 1/length(unique(dt_mzb_a$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("genera: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("a threshold") + 
        labs(col = "river type")
gg_sa_uniq_mzb_a_fam = 
        ggplot(dt_mzb_a, aes(x = threshold, y = u_all)) + 
        #geom_hline(yintercept = 1/length(unique(dt_mzb_a$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("families: Uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("a threshold") + 
        labs(col = "river type")

## uniqueness b
gg_sa_uniq_mzb_b = 
        ggplot(dt_mzb_b, aes(x = threshold, y = u_all)) + 
        #geom_hline(yintercept = 1/length(unique(dt_mzb_b$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("all levels: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("b threshold") + 
        labs(col = "river type")
gg_sa_uniq_mzb_b_spe = 
        ggplot(dt_mzb_b, aes(x = threshold, y = u_all)) + 
       # geom_hline(yintercept = 1/length(unique(dt_mzb_b$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("species: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("b threshold") + 
        labs(col = "river type")
gg_sa_uniq_mzb_b_gen = 
        ggplot(dt_mzb_b, aes(x = threshold, y = u_all)) + 
       # geom_hline(yintercept = 1/length(unique(dt_mzb_b$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("genera: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("b threshold") + 
        labs(col = "river type")
gg_sa_uniq_mzb_b_fol = 
        ggplot(dt_mzb_b, aes(x = threshold, y = u_all)) + 
      #  geom_hline(yintercept = 1/length(unique(dt_mzb_b$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("families: uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("b threshold") + 
        labs(col = "river type")

# combined ----------------------------------------------------------------
## -- richness - A - all levels -- #
ggrid_rich_a = plot_grid(gg_sa_rich_mzb_a     + theme(legend.position = "none"), 
                         gg_sa_rich_mzb_a_spe + theme(legend.position = "none"), 
                         gg_sa_rich_mzb_a_gen + theme(legend.position = "none"), 
                         gg_sa_rich_mzb_a_fol + theme(legend.position = "none"))
leg_rich_a = get_legend(
        gg_sa_rich_mzb_a + theme(legend.position = "bottom")
)
ggrid_rich_a_mzb = plot_grid(ggrid_rich_a, leg_rich_a, ncol = 1, rel_heights = c(1,.2))

## -- richness - A+B - taxa -- ## 
ggrid_rich_mzb = plot_grid(gg_sa_rich_mzb_a + theme(legend.position = "none") + ylim(c(0,250)) + ggtitle("Macroinvertebrates"), 
                       gg_sa_rich_mzb_b + theme(legend.position = "none") + ylab("") + ylim(c(0,250)) + ggtitle("")
                       ,labels = c("A", "B"))
leg_rich_mzb = get_legend(gg_sa_rich_mzb_a + theme(legend.position = "bottom"))
ggrid_rich_mzb = plot_grid(ggrid_rich_mzb, leg_rich_mzb, ncol = 1, rel_heights = c(1,.2))
## -- unique - A+B - taxa -- ## 
ggrid_uniq_mzb = plot_grid(gg_sa_uniq_mzb_a + theme(legend.position = "none") + ylim(c(0,1)) + ggtitle("Macroinvertebrates"), 
                           gg_sa_uniq_mzb_b + theme(legend.position = "none") + ylab("") + ylim(c(0,1)) + ggtitle("")
                           ,labels = c("A", "B"))
leg_uniq_mzb = get_legend(gg_sa_uniq_mzb_a + theme(legend.position = "bottom"))
ggrid_uniq_mzb = plot_grid(ggrid_uniq_mzb, leg_uniq_mzb, ncol = 1, rel_heights = c(1,.2))
