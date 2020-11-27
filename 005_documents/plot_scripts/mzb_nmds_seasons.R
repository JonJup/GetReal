### --- GETREAL 
### --- Working Package 2
### --- Macroinvertebrates
### --- Seasons NMDS plot for documents 


# setup -------------------------------------------------------------------
pacman::p_load(data.table, dplyr,here, magrittr, ggplot2, vegan)
dir_nmds = here("002_working_package_02/003_seasonality/003_results/invertebrates/nmds/")

# data ---------------------------------------------------------------
vg_nmds = readRDS(file.path(dir_nmds, "RT10_sub1_iter1000_not_converged.RDS"))
ch_seas = readRDS(file.path(dir_nmds, "../season_list.RDS"))
ch_seas = ch_seas[[19]]
dt_nmds = data.table(NMDS1 = scores(vg_nmds)[,1]  ,  NMDS2 = scores(vg_nmds)[,2]  , season = factor(ch_seas, levels = c("spring", "summer", "autumn", "winter")))
tb_hull = dt_nmds  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
my_color_palette = c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")
# plot  -------------------------------------------------------------------
gg_mz_nmds_season = 
        ggplot(data = dt_nmds,   aes(x = NMDS1, y = NMDS2)) +
        geom_polygon(data = tb_hull,   alpha = 0.5, aes(fill = season)) +
        geom_point(aes(fill = season), shape = 21) +
        ggtitle("NMDS subset of RT10") +
        labs(fil = "Season",
             subtitle = paste0("Macroinvertebrates, Stress: ", round(vg_nmds$stress,   2))) +
        scale_fill_manual(values = my_color_palette[c(1, 2, 4, 6)])

