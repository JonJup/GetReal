# -------------------------------------- #
### --- Sensitivity Analysis Plots --- ### 
# -------------------------------------- #

#date written: 05.11.20 

#GetReal 
#Working Package 2 
#Diatoms

# setup -------------------------------------------------------------------
if(!require(pacman))install.packages(pacman)
p_load(ggplot2,viridis)

dir_dia = "~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data"
dir_mzb = "~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data"

# load data ---------------------------------------------------------------
dt_dia_a = readRDS(file.path(dir_dia, "012_2020-11-05_sensitivity_parameter_a_50steps.RDS"))
dt_dia_b = readRDS(file.path(dir_dia, "012_2020-11-05_sensitivity_parameter_b_50steps.RDS"))

# create plots ------------------------------------------------------------
gg_sa_rich_dia_a = 
        ggplot(dt_dia_a, aes(x = threshold, y = n_all)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("Diatom taxon richness") + 
        scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("a threshold") + 
        theme(legend.position = "bottom")
gg_sa_rich_dia_b = 
        ggplot(dt_dia_b, aes(x = threshold, y = n_all)) +
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("Diatom taxon richness"  ) + 
        viridis::scale_color_viridis(discrete=T) + 
        ylab("richness") + 
        xlab("b threshold") + 
        theme(legend.position = "bottom")
gg_sa_uniq_dia_a = 
        ggplot(dt_dia_a, aes(x = threshold, y = u_all)) + 
       # geom_hline(yintercept = 1/length(unique(dt_dia_a$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("Uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("a threshold") + 
        labs(col = "river type")
gg_sa_uniq_dia_b = 
        ggplot(dt_dia_b, aes(x = threshold, y = u_all)) + 
       # geom_hline(yintercept = 1/length(unique(dt_dia_b$river_type)), linetype = 2) + 
        geom_line(size = 1.2, aes(col=river_type)) +
        ggtitle("Uniqueness") + 
        scale_color_viridis(discrete = T) + 
        ylab("uniqueness score") + 
        xlab("b threshold") + 
        labs(col = "river type")

# grid plots  -------------------------------------------------------------

## -- richness - A+B - taxa -- ## 
ggrid_rich_dia = plot_grid(
        gg_sa_rich_dia_a + theme(legend.position = "none") + ylim(c(0, 200)) + ggtitle("Diatoms"),
        gg_sa_rich_dia_b + theme(legend.position = "none") + ylab("") + ylim(c(0, 200)) + ggtitle(""),
        labels = c("C","D")
)
leg_rich_dia = get_legend(gg_sa_rich_dia_a + theme(legend.position = "bottom"))
ggrid_rich_dia = plot_grid(ggrid_rich_dia, leg_rich_dia, ncol = 1, rel_heights = c(1,.2))
## -- uniqueness - A+B - taxa -- ## 
ggrid_uniq_dia = plot_grid(
        gg_sa_uniq_dia_a + theme(legend.position = "none") + ylim(c(0, 1)) + ggtitle("Diatoms"),
        gg_sa_uniq_dia_b + theme(legend.position = "none") + ylab("") + ylim(c(0, 1)) + ggtitle(""),
        labels = c("C","D")
)
leg_uniq_dia = get_legend(gg_sa_uniq_dia_a + theme(legend.position = "bottom"))
ggrid_uniq_dia = plot_grid(ggrid_uniq_dia, leg_uniq_dia, ncol = 1, rel_heights = c(1,.2))
