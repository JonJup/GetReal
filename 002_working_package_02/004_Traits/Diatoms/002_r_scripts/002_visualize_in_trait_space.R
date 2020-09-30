### ---------------------------------------------------- ###
### --- Visualize typical assemblages in trait space --- ###
### ---------------------------------------------------- ###

# date written: 22.09.20 
# date changed: 23.09.20
# date used   : 22.09.20 + 23.  

# Project: Get Real - WP2 - Diatoms Traits 

# Use the diatom TAs with trait data to visualize TAs in trait space. 

# TODO TA mean values
# TODO Weight traits by B value 
# TODO Support Vector Machines 

# setup -------------------------------------------------------------------

pacman::p_load(cluster, data.table, dplyr, ggplot2, ggbeeswarm, here,  magrittr,  purrr, readxl, stringr, umap, vegan)
setwd(here())

source("002_WP2/004_Traits/Diatoms/002_r_scripts/f_001_boxplot_w_jitter.R")
# read data ---------------------------------------------------------------

dt_dia_trait_o <- readRDS("002_WP2/004_Traits/Diatoms/003_processed_data/2020-09-22_diatom_ta_w_traits.RDS")

# cleaning ----------------------------------------------------------------

dt_dia_trait_sub <- copy(dt_dia_trait_o)
# drop columns 
dt_dia_trait_sub <- dt_dia_trait_o[, c("sqrtIV", "n_taxa", "taxon2", "trait_id") := NULL]


# box plots  --------------------------------------------------------------

for (i in seq_along(names(dt_dia_trait_sub))) {
        
        if (names(dt_dia_trait_sub)[i] %in% c("taxon", "A", "B", "p_value", "ll_traits", "shape", "group"))
                next()
        if( i<=25) next()
        
        loop_plot1 <- bwjp(variable = names(dt_dia_trait_sub)[i]) 
        loop_plot2 <- bwjp(variable = names(dt_dia_trait_sub)[i], nogen = T) 
        loop_plot3 <- bwjp(variable = names(dt_dia_trait_sub)[i], nodub = T) 
        loop_plot4 <- bwjp(variable = names(dt_dia_trait_sub)[i], nogen = T, nodub = T) 
        
        loop_plot1 <- loop_plot1 + ggtitle(paste(names(dt_dia_trait_sub)[i]), "all taxa")
        loop_plot2 <- loop_plot2 + ggtitle(paste(names(dt_dia_trait_sub)[i]), "no genus trait data")
        loop_plot3 <- loop_plot3 + ggtitle(paste(names(dt_dia_trait_sub)[i]), "only unique taxa")
        loop_plot4 <- loop_plot4 + ggtitle(paste(names(dt_dia_trait_sub)[i]), "no genus trait data + only unique taxa")
        
        number <- i-6
        
        if(number < 10) number <- paste0("00",number)
        if(number >  9) number <- paste0("0", number)
        
        if (names(dt_dia_trait_sub)[i] == "length/width_ratio") names(dt_dia_trait_sub)[i] <- "length_width_ratio"
        
        ggsave(filename = paste0("002_WP2/004_Traits/Diatoms/004_plots/trait_boxplots/", number,"_", Sys.Date(), "_", names(dt_dia_trait_sub)[i], "_all.jpeg")        , plot = loop_plot1)
        ggsave(filename = paste0("002_WP2/004_Traits/Diatoms/004_plots/trait_boxplots/", number,"_", Sys.Date(), "_", names(dt_dia_trait_sub)[i], "_nogen.jpeg")      , plot = loop_plot2)
        ggsave(filename = paste0("002_WP2/004_Traits/Diatoms/004_plots/trait_boxplots/", number,"_", Sys.Date(), "_", names(dt_dia_trait_sub)[i], "_nodub.jpeg")      , plot = loop_plot3)
        ggsave(filename = paste0("002_WP2/004_Traits/Diatoms/004_plots/trait_boxplots/", number,"_", Sys.Date(), "_", names(dt_dia_trait_sub)[i], "_nogen_nodub.jpeg"), plot = loop_plot4)
        
        print(i)
        
        rm(loop_plot1, loop_plot2, loop_plot3, loop_plot4, number, i)
}
rm(bwjp);gc()


# TA mean values  ---------------------------------------------------------

#greb

dt_ta_means       <- copy(dt_dia_trait_sub)
dt_ta_means[, c(2:4,51) := NULL]
dt_ta_means_nogen <- copy(dt_ta_means)
dt_ta_means_nogen <- dt_ta_means_nogen[is.na(ll_traits)]
dt_ta_mean_nono   <- copy(dt_ta_means_nogen)


loop_list <- list()

for (i in 1:length(unique(dt_ta_means$group))) {
        loop_var <- unique(dt_ta_means$group)[i]
        dt_loop_sub <- dt_ta_means[group == loop_var]
        
        dt_loop_mean   <- map_df(.x = dt_loop_sub[,-(1:3)], .f = mean)   %>% setDT
        dt_loop_median <- map_df(.x = dt_loop_sub[,-(1:3)], .f = median) %>% setDT
        dt_loop_sd     <- map_df(.x = dt_loop_sub[,-(1:3)], .f = sd)     %>% setDT
        
        dt_loop <- rbindlist(l = list(dt_loop_mean, dt_loop_median, dt_loop_sd))
        dt_loop[, river_type := loop_var]
        dt_loop[, statistic := c("mean", "median", "sd")]
        loop_list[[i]] <- dt_loop
        
        # dt_loop_new <- data.table(rt = rep(loop_var, 132), 
        #                           variable = rep(names(dt_loop_sub[,-(1:3)]), 3), 
        #                           statistic = rep(c("mean", "median", "standard_deviation"), each = 44),
        #                           value = append(vec_loop_mean, vec_loop_median, vec_loop_sd))

        print(i)
}

dt_means <- rbindlist(loop_list)

min_max_var <- c(min(dt_dia_trait_sub$salinity_value), max(dt_dia_trait_sub$salinity_value))

plot_base <- ggplot(data = filter(dt_means, statistic == "mean"), aes(x = river_type, y = salinity_value, col = river_type)) + geom_point() + ylim(min_max_var[1],min_max_var[2])
plot_base <- plot_base +  geom_pointrange(ymin = pull(select(filter(dt_means, statistic == "mean"), salinity_value) - 
                                     select(filter(dt_means, statistic == "sd"), salinity_value)), 
                             ymax =  pull(select(filter(dt_means, statistic == "mean"), salinity_value) + 
                                     select(filter(dt_means, statistic == "sd"), salinity_value)))
plot_base <- plot_base + geom_point(data = filter(dt_means, statistic == "median"), aes(x = river_type, y = salinity_value, col = river_type), shape = 17, size = 2)
plot_base <- plot_base +
        theme_minimal() +
        theme(
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
        ) + 
        ggtitle(paste(variable), "all taxa")
# NMDS plots  -------------------------------------------------------------

dt_NMDS <- copy(dt_dia_trait_sub)


dt_NMDS <- dt_NMDS[,c("taxon", "group", "ll_traits","high_profile_guild", "low_profile_guild", "motile_guild", "euplanctonic_guild", 
                      "benthic", "planctonic", "mobile", "length", "width", "thickness", "biovolume", 
                      "length/width_ratio")]
dt_NMDS$high_profile_guild %<>% factor()
dt_NMDS$low_profile_guild %<>% factor()
dt_NMDS$motile_guild %<>% factor()
dt_NMDS$high_profile_guild %<>% factor()
dt_NMDS$euplanctonic_guild %<>% factor()
dt_NMDS$benthic %<>% factor()
dt_NMDS$planctonic %<>% factor()
dt_NMDS$mobile %<>% factor()

dt_NMDS_uni <- unique(dt_NMDS, by = "taxon")
dt_NMDS_nogen  <- dt_NMDS[is.na(ll_traits)]

dist_NMDS   <- daisy(x = dt_NMDS[,-c(1:3)], metric = "gower")
dist_NMDS_u <- daisy(x = dt_NMDS_uni[,-c(1:3)], metric = "gower")
dist_NMDS_n <- daisy(x = dt_NMDS_nogen[,-c(1:3)], metric = "gower")


obj_NMDS  <- metaMDS(dist_NMDS)
obj_NMDSu <- metaMDS(dist_NMDS_u)
obj_NMDSn <- metaMDS(dist_NMDS_n)


nmds_data   <- data.table(NMDS1 = scores(obj_NMDS)[,1],  NMDS2 = scores(obj_NMDS)[,2], river_type = factor(dt_NMDS$group))
nmds_data_u <- data.table(NMDS1 = scores(obj_NMDSu)[,1],  NMDS2 = scores(obj_NMDSu)[,2], river_type = factor(dt_NMDS_uni$group))
nmds_data_n <- data.table(NMDS1 = scores(obj_NMDSn)[,1],  NMDS2 = scores(obj_NMDSn)[,2], river_type = factor(dt_NMDS_nogen$group))


hull      <- nmds_data %>% group_by(river_type) %>% slice(chull(NMDS1, NMDS2))
hull      <- nmds_data_u %>% group_by(river_type) %>% slice(chull(NMDS1, NMDS2))
hull      <- nmds_data_n %>% group_by(river_type) %>% slice(chull(NMDS1, NMDS2))

#plot_NMDS <- ggplot(data = nmds_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull, alpha = 0.5, aes(fill = river_type)) + geom_point(aes(fill = river_type), shape = 21) + ggtitle(paste0("NMDS",   "- Stress: ", round(obj_NMDS$stress, 3))) + labs(fil = "river type") #+ scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_NMDS <- ggplot(data = nmds_data, aes(x = NMDS1, y = NMDS2)) +   geom_point(aes(fill = river_type), shape = 21, size = 2) + ggtitle(paste0("NMDS",   "- Stress: ", round(obj_NMDS$stress, 3))) + labs(fil = "river type") #+ scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_NMDS <- ggplot(data = nmds_data_u, aes(x = NMDS1, y = NMDS2)) +   geom_point(aes(fill = river_type), shape = 21, size = 2) + ggtitle(paste0("NMDS",   "- Stress: ", round(obj_NMDS$stress, 3))) + labs(fil = "river type") #+ scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_NMDS <- ggplot(data = nmds_data_n, aes(x = NMDS1, y = NMDS2)) +   geom_point(aes(fill = river_type), shape = 21, size = 2) + ggtitle(paste0("NMDS",   "- Stress: ", round(obj_NMDS$stress, 3))) + labs(fil = "river type") #+ scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
