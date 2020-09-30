# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Diatoms ---------- ---------- ### 
# ------------------------------------- #

# date: 02.09.20 + 7. + 8. + 15. + 16. 
# GetReal WP 2 
# Diatoms 

# Look for differences between seasons using NMDS 

# setup -------------------------------------------------------------------

pacman::p_load(here, data.table, dplyr, fuzzySim, ggplot2, magrittr, stringr, vegan)
library(plotly)
setwd(here())

# load and prepare data ---------------------------------------------------------------

files <- dir(path = "003_results/diatoms/001_speciesXsites_tables/")
#files <- files[grep(pattern = "2020-09-14", x = files)]
for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt[0-9]*_[a-z]*")
        assign(x     = obj_name,
               value = readRDS(paste0("003_results/diatoms/001_speciesXsites_tables/",lv)))
        
}
rm(i, lv, obj_name, files);gc()
file_l <- ls()

## --  join tables -- ## 
loop_over_var <- c("08","10","11","14", "15", "16", "17", "18", "19")
for (i in loop_over_var) {
        
        file_pre <- paste0("rt", i, "_")
        files    <- ls()[grepl(pattern = file_pre, x = ls())]
                ld1 <- get(files[1]) 
                ld2 <- get(files[2])
                ldj <- ld2[ld1, on = "gr_sample_id"]
                if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 1"))}
                rm(ld1, ld2);gc()
                
        assign(x = paste0(file_pre, "all"), 
               value = ldj)
        print(i)
        rm(ldj, files, file_pre, i);gc()
        
}
rm(list = file_l)
rm(file_l, loop_over_var);gc()


# Create distance matrix --------------------------------------------------

files <- ls()

if (nrow(rt08_all[is.na(season)]) != 0 |
    nrow(rt10_all[is.na(season)]) != 0 |
    nrow(rt11_all[is.na(season)]) != 0 |
    nrow(rt14_all[is.na(season)]) != 0 |
    nrow(rt15_all[is.na(season)]) != 0 |
    nrow(rt16_all[is.na(season)]) != 0 |
    nrow(rt17_all[is.na(season)]) != 0 |
    nrow(rt18_all[is.na(season)]) != 0 |
    nrow(rt19_all[is.na(season)]) != 0) {
        print("Quality Check Failed")
} else {
        print("Quality Check Passed")
}

# remove i.season column that was created during the join
for (i in files){
        ld <- get(i)
        ld[,i.season := NULL]
        assign(x = i, 
               value = ld)
        rm(ld, i);gc()
}

# loop to create distance matrices 
for (i in files){
        ld             <- get(i)
        ld             <- t(ld)
        colnames(ld)   <- ld[1,]
        ld             <- ld[-1,]
        seasons_var    <- ld[1,]
        ld             <- ld[-1,]
        taxa_names     <- rownames(ld)
        ld             <- apply(ld, 2, as.numeric)
        rownames(ld)   <- taxa_names
        dld           <- 1 - simMat(ld, method = "Jaccard")
        assign(x = paste0("d_", i), value = dld)
        assign(x = paste0("season_", i), value = seasons_var)
        print(i)
        rm(ld, dld,i, seasons_var);gc()
        
}


# run NMDS ----------------------------------------------------------------

# nmds08 <- metaMDS(comm = d_rt08_all, try = 500, k = 2)
# nmds08 <- metaMDS(comm = d_rt08_all, try = 1000, k = 2, previous.best = nmds08)
# nmds08 <- metaMDS(comm = d_rt08_all, try = 5000, k = 2, previous.best = nmds08) #solution reached
nmds08 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt08.RDS")

# nmds10 <- metaMDS(comm = d_rt10_all, try = 500, k = 2)
# nmds10 <- metaMDS(comm = d_rt10_all, try = 1000, k = 2, previous.best = nmds10)
# nmds10 <- metaMDS(comm = d_rt10_all, try = 5000, k = 2, previous.best = nmds10) # still no convergence but I stick with it. 
nmds10 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt10.RDS")

# nmds11 <- metaMDS(comm = d_rt11_all, try = 500, k = 2)
# nmds11 <- metaMDS(comm = d_rt11_all, try = 500, k = 2, previous.best = nmds11) #solution reached
nmds11 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt11.RDS")

# nmds14 <- metaMDS(comm = d_rt14_all, try = 500, k = 2)
# nmds14 <- metaMDS(comm = d_rt14_all, try = 1000, k = 2, previous.best = nmds14)
# nmds14 <- metaMDS(comm = d_rt14_all, try = 5000, k = 2, previous.best = nmds14) # still no convergence but I stick with it.
nmds14 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt14.RDS")

# nmds15 <- metaMDS(comm = d_rt15_all, try = 500, k = 2)
# nmds15 <- metaMDS(comm = d_rt15_all, try = 1000, k = 2, previous.best = nmds15)
# nmds15 <- metaMDS(comm = d_rt15_all, try = 5000, k = 2, previous.best = nmds15) # still no convergence but I stick with it.
nmds15 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt15.RDS")

#nmds16 <- metaMDS(comm = d_rt16_all, try = 500, k = 2) # no convergence
nmds16 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt16.RDS")
nmds16 <- metaMDS(comm = d_rt16_all, try = 1000, k = 2, previous.best = nmds16) # solution reached. 

#nmds17 <- metaMDS(comm = d_rt17_all, try = 500, k = 2) # no convergence
nmds17 <- readRDS("003_results/diatoms/003_nmds/2020-09-15_nmds_object_diatoms_rt17.RDS")
nmds17 <- metaMDS(comm = d_rt17_all, try = 1000, k = 2, previous.best = nmds17) # no convergence

nmds18 <- metaMDS(comm = d_rt18_all, try = 500, k = 2) # no convergence
nmds18 <- metaMDS(comm = d_rt18_all, try = 1000, k = 2, previous.best = nmds18) # no convergence 
 

nmds19 <- metaMDS(comm = d_rt19_all, try = 500, k = 2) # no convergence
nmds19 <- metaMDS(comm = d_rt19_all, try = 1000, k = 2, previous.best = nmds19) # no convergence
nmds19 <- metaMDS(comm = d_rt19_all, try = 5000, k = 2, previous.best = nmds19) # no convergence

# Test 3d NMDS 

nmds3d_08 <- metaMDS(comm = d_rt08_all, try = 500, k = 3)

# plot nmds ---------------------------------------------------------------

# 2d
nmds08_data <- data.table(NMDS1 = scores(nmds08)[,1],  NMDS2 = scores(nmds08)[,2], season = factor(season_rt08_all))
nmds10_data <- data.table(NMDS1 = scores(nmds10)[,1],  NMDS2 = scores(nmds10)[,2], season = factor(season_rt10_all))
nmds11_data <- data.table(NMDS1 = scores(nmds11)[,1],  NMDS2 = scores(nmds11)[,2], season = factor(season_rt11_all))
nmds14_data <- data.table(NMDS1 = scores(nmds14)[,1],  NMDS2 = scores(nmds14)[,2], season = factor(season_rt14_all))
nmds15_data <- data.table(NMDS1 = scores(nmds15)[,1],  NMDS2 = scores(nmds15)[,2], season = factor(season_rt15_all))
nmds16_data <- data.table(NMDS1 = scores(nmds16)[,1],  NMDS2 = scores(nmds16)[,2], season = factor(season_rt16_all))
nmds17_data <- data.table(NMDS1 = scores(nmds17)[,1],  NMDS2 = scores(nmds17)[,2], season = factor(season_rt17_all))
nmds18_data <- data.table(NMDS1 = scores(nmds18)[,1],  NMDS2 = scores(nmds18)[,2], season = factor(season_rt18_all))
nmds19_data <- data.table(NMDS1 = scores(nmds19)[,1],  NMDS2 = scores(nmds19)[,2], season = factor(season_rt19_all))
# 3d
nmds3d_08_data <- data.table(NMDS1 = scores(nmds3d_08)[,1],  NMDS2 = scores(nmds3d_08)[,2],NMDS3 = scores(nmds3d_08)[,3], season = factor(season_rt08_all))

plot_ly(x=nmds3d_08_data$NMDS1, y=nmds3d_08_data$NMDS2, z=nmds3d_08_data$NMDS3, type="scatter3d", mode="markers", color=nmds3d_08_data$season)

# hulls  2d
hull_08 <- nmds08_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_10 <- nmds10_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_11 <- nmds11_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_14 <- nmds14_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_15 <- nmds15_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_16 <- nmds16_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_17 <- nmds17_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_18 <- nmds18_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_19 <- nmds19_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))

# color palette 
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

# create plot objects 
plot_withhull_nmds08 <- ggplot(data = nmds08_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_08, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT08",   "- Stress: ", round(nmds08$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds10 <- ggplot(data = nmds10_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_10, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT10",   "- Stress: ", round(nmds10$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds11 <- ggplot(data = nmds11_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_11, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT11",   "- Stress: ", round(nmds11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds14 <- ggplot(data = nmds14_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_14, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT14",   "- Stress: ", round(nmds14$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds15 <- ggplot(data = nmds15_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_15, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT15",   "- Stress: ", round(nmds15$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds16 <- ggplot(data = nmds16_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_16, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT16",   "- Stress: ", round(nmds16$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds17 <- ggplot(data = nmds17_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_17, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT17",   "- Stress: ", round(nmds17$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds18 <- ggplot(data = nmds18_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_18, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT18",   "- Stress: ", round(nmds18$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds19 <- ggplot(data = nmds19_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_19, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT19",   "- Stress: ", round(nmds19$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_wohull_nmds1    <- ggplot(data = nmds1_data,  aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT1+18", "- Stress: ", round(nmds1$stress,  2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_wohull_nmds2    <- ggplot(data = nmds2_data,  aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT2+8",  "- Stress: ", round(nmds2$stress,  2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds5    <- ggplot(data = nmds5_data,  aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT5",    "- Stress: ", round(nmds5$stress,  2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds10   <- ggplot(data = nmds10_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT10",   "- Stress: ", round(nmds10$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds11   <- ggplot(data = nmds11_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT11",   "- Stress: ", round(nmds11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds14   <- ggplot(data = nmds14_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT14",   "- Stress: ", round(nmds14$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds15   <- ggplot(data = nmds15_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT15",   "- Stress: ", round(nmds15$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds16   <- ggplot(data = nmds16_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT16",   "- Stress: ", round(nmds16$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds17   <- ggplot(data = nmds17_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT17",   "- Stress: ", round(nmds17$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wohull_nmds19   <- ggplot(data = nmds19_data, aes(x = NMDS1, y = NMDS2)) +                                                                  geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of diatom communities in RT19",   "- Stress: ", round(nmds19$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_onlyhull_nmds1  <- ggplot(data = nmds1_data,  aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_1,  alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT1+18", "- Stress: ", round(nmds1$stress,  2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds2  <- ggplot(data = nmds2_data,  aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_2,  alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT2+8",  "- Stress: ", round(nmds2$stress,  2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds5  <- ggplot(data = nmds5_data,  aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_5,  alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT5",    "- Stress: ", round(nmds5$stress,  2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds10 <- ggplot(data = nmds10_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_10, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT10",   "- Stress: ", round(nmds10$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds11 <- ggplot(data = nmds11_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_11, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT11",   "- Stress: ", round(nmds11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds14 <- ggplot(data = nmds14_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_14, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT14",   "- Stress: ", round(nmds14$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds15 <- ggplot(data = nmds15_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_15, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT15",   "- Stress: ", round(nmds15$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds16 <- ggplot(data = nmds16_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_16, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT16",   "- Stress: ", round(nmds16$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds17 <- ggplot(data = nmds17_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_17, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT17",   "- Stress: ", round(nmds17$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds19 <- ggplot(data = nmds19_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_19, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of diatom communities in RT19",   "- Stress: ", round(nmds19$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])


# save to file  -----------------------------------------------------------
# save plot objects - pdf 
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt1.pdf"  , plot = plot_withhull_nmds1)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt1.pdf" , plot = plot_wohull_nmds1)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt1.pdf"  , plot = plot_onlyhull_nmds1)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt2.pdf"  , plot = plot_withhull_nmds2)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt2.pdf" , plot = plot_wohull_nmds2)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt2.pdf"  , plot = plot_onlyhull_nmds2)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt5.pdf"  , plot = plot_withhull_nmds5)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt5.pdf" , plot = plot_wohull_nmds5)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt5.pdf"  , plot = plot_onlyhull_nmds5)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt10.pdf" , plot = plot_withhull_nmds10)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt10.pdf", plot = plot_wohull_nmds10)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt10.pdf" , plot = plot_onlyhull_nmds10)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt11.pdf" , plot = plot_withhull_nmds11)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt11.pdf", plot = plot_wohull_nmds11)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt11.pdf" , plot = plot_onlyhull_nmds11)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt14.pdf" , plot = plot_withhull_nmds14)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt14.pdf", plot = plot_wohull_nmds14)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt14.pdf" , plot = plot_onlyhull_nmds14)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt15.pdf" , plot = plot_withhull_nmds15)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt15.pdf", plot = plot_wohull_nmds15)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt15.pdf" , plot = plot_onlyhull_nmds15)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt16.pdf" , plot = plot_withhull_nmds16)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt16.pdf", plot = plot_wohull_nmds16)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt16.pdf" , plot = plot_onlyhull_nmds16)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt17.pdf" , plot = plot_withhull_nmds17)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt17.pdf", plot = plot_wohull_nmds17)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt17.pdf" , plot = plot_onlyhull_nmds17)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_wh_rt19.pdf" , plot = plot_withhull_nmds19)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_woh_rt19.pdf", plot = plot_wohull_nmds19)
# ggsave(filename = "004_plots/diatoms/nmds/nmds_oh_rt19.pdf" , plot = plot_onlyhull_nmds19)
# save plot objects - jpeg


ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt08.jpeg"  , plot = plot_withhull_nmds08)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt10.jpeg"  , plot = plot_withhull_nmds10)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt11.jpeg"  , plot = plot_withhull_nmds11)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt14.jpeg"  , plot = plot_withhull_nmds14)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt15.jpeg"  , plot = plot_withhull_nmds15)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt16.jpeg"  , plot = plot_withhull_nmds16)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt17.jpeg"  , plot = plot_withhull_nmds17)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt18.jpeg"  , plot = plot_withhull_nmds18)
ggsave(filename =   "004_plots/diatoms/nmds/nmds_rt19.jpeg"  , plot = plot_withhull_nmds19)




# save nmds R-objects 
saveRDS(nmds08,  paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt08.RDS"))
saveRDS(nmds10, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt10.RDS"))
saveRDS(nmds11, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt11.RDS"))
saveRDS(nmds14, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt14.RDS"))
saveRDS(nmds15, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt15.RDS"))
saveRDS(nmds16, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt16.RDS"))
saveRDS(nmds17, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt17.RDS"))
saveRDS(nmds18, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt18.RDS"))
saveRDS(nmds19, paste0("003_results/diatoms/003_nmds/", Sys.Date(), "_nmds_object_diatoms_rt19.RDS"))

## -- ##  
if (readline("remove all? ") == "yes") rm(list = ls())

