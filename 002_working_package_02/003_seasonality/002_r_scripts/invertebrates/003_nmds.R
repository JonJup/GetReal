# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Macroinvertebrates ---------- ### 
# ------------------------------------- #

# date: 01.09.20 + 10.09
# GetReal WP 2 MZB


# setup -------------------------------------------------------------------

pacman::p_load(data.table, dplyr, fuzzySim, ggplot2, here, stringr, vegan)
setwd(here())

# load data ---------------------------------------------------------------

files <- dir(path = "003_results/invertebrates/001_speciesXsites_tables/")
files <- files[grepl(pattern = "2020-09-10", x = files)]
for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt.*") %>% 
            str_remove(pattern = "\\.RDS")
        assign(x     = obj_name,
               value = readRDS(paste0("003_results/invertebrates/001_speciesXsites_tables/",lv)))
        
}
rm(i, lv, obj_name, files);gc()
file_l <- ls()
# join tables 
loop_over_var <- c("1.1", "1.2","2_3", "4_5", "8_9", "10_11", "15_16", "17", "18", "19")
for (i in loop_over_var) {
        
        file_pre <- paste0("rt", i, "_")
        files    <- ls()[grepl(pattern = file_pre, x = ls())]

        ld1 <- get(files[1]) 
        ld2 <- get(files[2]) 
        ld3 <- get(files[3])  
        ldj <- ld3[ld2, on = "gr_sample_id"]
        if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 2"))}
        #ldf[, i.season := NULL]
        ldj <- ld1[ldj, on = "gr_sample_id"]
        if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 3"))}
        rm(ld1, ld2, ld3);gc()
        assign(x = paste0(file_pre, "all"), 
               value = ldj)
        print(paste(i, "finished"))
        rm(ldj, files, file_pre, i);gc()
        
}
rm(list = file_l)
rm(file_l, loop_over_var)


# fix combined tables -----------------------------------------------------

## -- distance matrix -- ##
files <- ls()

for (i in seq_along(files)) {
    ld <- get(files[i])
    seas_id <- which(str_detect(string = names(ld), pattern = "season"))
    ld[is.na(season) & !is.na(i.season), season := i.season]
    ld[, i.season := NULL]
    if (length(seas_id) > 2){
        ld[is.na(season) & !is.na(i.season.1), season := i.season.1]
        ld[, i.season.1 := NULL]
    }
    assign(x = files[i],
           value = ld)
}

# quality check - problematic data set number is printed. Best of nothing is returned 
for (i in seq_along(files)) {
    ld <- get(files[i])
    n  <- nrow(ld[is.na(season)])
    if (n != 0) print(i)
}

# fix NAs with code from Matt Dowle (https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table)
for (i in seq_along(files)) {
    ld <- get(files[i])
    for (j in seq_len(ncol(ld)))
        set(ld, which(is.na(ld[[j]])), j, 0)
}

# create distance matrices 
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

nmds1.1   <- metaMDS(comm = d_rt1.1_all,   try = 500, k = 2)  ; saveRDS(nmds1.1  , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt1.1.RDS"))
nmds1.2   <- metaMDS(comm = d_rt1.2_all,   try = 500, k = 2)  ; saveRDS(nmds1.2  , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt1.2.RDS")) 
nmds2_3   <- metaMDS(comm = d_rt2_3_all,   try = 500, k = 2)  ; saveRDS(nmds2_3  , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt2_3.RDS"))
nmds4_5   <- metaMDS(comm = d_rt4_5_all,   try = 5000, k = 2) ; saveRDS(nmds4_5  , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt4_5.RDS"))
nmds8_9   <- metaMDS(comm = d_rt8_9_all,   try = 5000, k = 2)  ; saveRDS(nmds8_9  , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt8_9.RDS"))
nmds10_11 <- metaMDS(comm = d_rt10_11_all, try = 1500, k = 2); saveRDS(nmds10_11, paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt10_11.RDS"))
nmds10_11 <- metaMDS(comm = d_rt10_11_all, try = 1500, k = 2, previous.best = nmds10_11); saveRDS(nmds10_11, paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt10_11.RDS"))
nmds15_16 <- metaMDS(comm = d_rt15_16_all, try = 500, k = 2); saveRDS(nmds15_16, paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt15_16.RDS"))
nmds17    <- metaMDS(comm = d_rt17_all,    try = 1000, k = 2)  ; saveRDS(nmds17   , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt17.RDS"))
nmds18    <- metaMDS(comm = d_rt18_all,    try = 1000, k = 2)  ; saveRDS(nmds18   , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt18.RDS")) 
nmds18    <- metaMDS(comm = d_rt18_all,    try = 1000, k = 2, previous.best = nmds18)  ; saveRDS(nmds18   , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt18.RDS")) 
nmds19    <- metaMDS(comm = d_rt19_all,    try = 500, k = 2)   ; saveRDS(nmds19   , paste0("003_results/invertebrates/004_nmds/", Sys.Date(), "_nmds_object_invertebrates_rt19.RDS"))


nmds1.1     <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt1.1.RDS")
nmds1.2     <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt1.2.RDS")
nmds2_3     <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt2_3.RDS")
nmds4_5     <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt4_5.RDS")
nmds8_9     <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt8_9.RDS")
nmds10_11   <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt10_11.RDS") 
nmds15_16   <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt15_16.RDS")
nmds17      <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt17.RDS")
nmds18      <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt18.RDS")
nmds19      <- readRDS("003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt19.RDS")



# data for plots ----------------------------------------------------------
nmds1.1_data   <- data.table(NMDS1 = scores(nmds1.1)[,1]  ,  NMDS2 = scores(nmds1.1)[,2]  , season = factor(season_rt1.1_all, levels = c("spring", "summer", "autumn", "winter")))
nmds1.2_data   <- data.table(NMDS1 = scores(nmds1.2)[,1]  ,  NMDS2 = scores(nmds1.2)[,2]  , season = factor(season_rt1.2_all, levels = c("spring", "summer", "autumn", "winter")))
nmds2_3_data   <- data.table(NMDS1 = scores(nmds2_3)[,1]  ,  NMDS2 = scores(nmds2_3)[,2]  , season = factor(season_rt2_3_all, levels = c("spring", "summer", "autumn", "winter")))
nmds4_5_data   <- data.table(NMDS1 = scores(nmds4_5)[,1]  ,  NMDS2 = scores(nmds4_5)[,2]  , season = factor(season_rt4_5_all, levels = c("spring", "summer", "autumn", "winter")))
nmds8_9_data   <- data.table(NMDS1 = scores(nmds8_9)[,1]  ,  NMDS2 = scores(nmds8_9)[,2]  , season = factor(season_rt8_9_all, levels = c("spring", "summer", "autumn", "winter")))
nmds10_11_data <- data.table(NMDS1 = scores(nmds10_11)[,1],  NMDS2 = scores(nmds10_11)[,2], season = factor(season_rt10_11_all, levels = c("spring", "summer", "autumn", "winter")))
nmds15_16_data <- data.table(NMDS1 = scores(nmds15_16)[,1],  NMDS2 = scores(nmds15_16)[,2], season = factor(season_rt15_16_all, levels = c("spring", "summer", "autumn", "winter")))
nmds17_data    <- data.table(NMDS1 = scores(nmds17)[,1]   ,  NMDS2 = scores(nmds17)[,2]   , season = factor(season_rt17_all,    levels = c("spring", "summer", "autumn", "winter")))
nmds18_data    <- data.table(NMDS1 = scores(nmds18)[,1]   ,  NMDS2 = scores(nmds18)[,2]   , season = factor(season_rt18_all,    levels = c("spring", "summer", "autumn", "winter")))
nmds19_data    <- data.table(NMDS1 = scores(nmds19)[,1]   ,  NMDS2 = scores(nmds19)[,2]   , season = factor(season_rt19_all,    levels = c("spring", "summer", "autumn", "winter")))

### ---  hulls --- ###  
hull_1.1   <- nmds1.1_data  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_1.2   <- nmds1.2_data  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_2_3   <- nmds2_3_data  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_4_5   <- nmds4_5_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_8_9   <- nmds8_9_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_10_11 <- nmds10_11_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_15_16 <- nmds15_16_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_17    <- nmds17_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_18    <- nmds18_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
hull_19    <- nmds19_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))

# color palette # 
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

## -- plots -- ## 
# create plot objects 
plot_withhull_nmds1.1   <- ggplot(data = nmds1.1_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_1.1,   alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.1",   "- Stress: ", round(nmds1.1$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds1.2   <- ggplot(data = nmds1.2_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_1.2,   alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.2",   "- Stress: ", round(nmds1.2$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds2_3   <- ggplot(data = nmds2_3_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_2_3,   alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT2_3",   "- Stress: ", round(nmds2_3$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds4_5   <- ggplot(data = nmds4_5_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_4_5,   alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT4_5",   "- Stress: ", round(nmds4_5$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds8_9   <- ggplot(data = nmds8_9_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_8_9,   alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT8_9",   "- Stress: ", round(nmds8_9$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds10_11 <- ggplot(data = nmds10_11_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_10_11, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT10_11", "- Stress: ", round(nmds10_11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds15_16 <- ggplot(data = nmds15_16_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_15_16, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT15_16", "- Stress: ", round(nmds15_16$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds17    <- ggplot(data = nmds17_data,    aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_17,    alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT17",    "- Stress: ", round(nmds17$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds18    <- ggplot(data = nmds18_data,    aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_18,    alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT18",    "- Stress: ", round(nmds18$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
plot_withhull_nmds19    <- ggplot(data = nmds19_data,    aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_19,    alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT19",    "- Stress: ", round(nmds19$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_wouthull_nmds1.1   <- ggplot(data = nmds1.1_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.1",   "- Stress: ", round(nmds1.1$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])# plot_wouthull_nmds1.1   <- ggplot(data = nmds1.1_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.1",   "- Stress: ", round(nmds1.1$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_wouthull_nmds1.2   <- ggplot(data = nmds1.2_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.2",   "- Stress: ", round(nmds1.2$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds1.2   <- ggplot(data = nmds1.2_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.2",   "- Stress: ", round(nmds1.2$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds2_3   <- ggplot(data = nmds2_3_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT2_3",   "- Stress: ", round(nmds2_3$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds4_5   <- ggplot(data = nmds4_5_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT4_5",   "- Stress: ", round(nmds4_5$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds8_9   <- ggplot(data = nmds8_9_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT8_9",   "- Stress: ", round(nmds8_9$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) # plot_wouthull_nmds8_9   <- ggplot(data = nmds8_9_data,   aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT8_9",   "- Stress: ", round(nmds8_9$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds10_11 <- ggplot(data = nmds10_11_data, aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT10_11", "- Stress: ", round(nmds10_11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds15_16 <- ggplot(data = nmds15_16_data, aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT15_16", "- Stress: ", round(nmds15_16$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds17    <- ggplot(data = nmds17_data,    aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT17",    "- Stress: ", round(nmds17$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds18    <- ggplot(data = nmds18_data,    aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT18",    "- Stress: ", round(nmds18$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_wouthull_nmds19    <- ggplot(data = nmds19_data,    aes(x = NMDS1, y = NMDS2)) +                                                                     geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT19",    "- Stress: ", round(nmds19$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)]) 
# plot_onlyhull_nmds1.1   <- ggplot(data = nmds1.1_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_1.1,   alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.1",   "- Stress: ", round(nmds1.1$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds1.2   <- ggplot(data = nmds1.2_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_1.2,   alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT1.2",   "- Stress: ", round(nmds1.2$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds2_3   <- ggplot(data = nmds2_3_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_2_3,   alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT2_3",   "- Stress: ", round(nmds2_3$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds4_5   <- ggplot(data = nmds4_5_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_4_5,   alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT4_5",   "- Stress: ", round(nmds4_5$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds8_9   <- ggplot(data = nmds8_9_data,   aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_8_9,   alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT8_9",   "- Stress: ", round(nmds8_9$stress,   2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds10_11 <- ggplot(data = nmds10_11_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_10_11, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT10_11", "- Stress: ", round(nmds10_11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds15_16 <- ggplot(data = nmds15_16_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_15_16, alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT15_16", "- Stress: ", round(nmds15_16$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds17    <- ggplot(data = nmds17_data,    aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_17,    alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT17",    "- Stress: ", round(nmds17$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds18    <- ggplot(data = nmds18_data,    aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_18,    alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT18",    "- Stress: ", round(nmds18$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# plot_onlyhull_nmds19    <- ggplot(data = nmds19_data,    aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_19,    alpha = 0.5, aes(fill = season)) +                                              ggtitle(paste0("NMDS of macroinvertebrate communities in RT19",    "- Stress: ", round(nmds19$stress,    2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
# -- ## 
# save plots to file (jpeg) 
ggsave(filename = "004_plots/invertebrates/nmds/nmds_rivertype_1_subset1.jpeg" , plot = plot_withhull_nmds1.1)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt1.1.jpeg" , plot = plot_wouthull_nmds1.1)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt1.1.jpeg" , plot = plot_onlyhull_nmds1.1)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_rivertype_1_subset2.jpeg" , plot = plot_withhull_nmds1.2)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt1.2.jpeg" , plot = plot_wouthull_nmds1.2)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt1.2.jpeg" , plot = plot_onlyhull_nmds1.2)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_2_3.jpeg" , plot = plot_withhull_nmds2_3)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt2_3.jpeg" , plot = plot_wouthull_nmds2_3)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt2_3.jpeg" , plot = plot_onlyhull_nmds2_3)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_4_5.jpeg", plot = plot_withhull_nmds4_5)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt4_5.jpeg", plot = plot_wouthull_nmds4_5)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt4_5.jpeg", plot = plot_onlyhull_nmds4_5)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_8_9.jpeg", plot = plot_withhull_nmds8_9)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt8_9.jpeg", plot = plot_wouthull_nmds8_9)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt8_9.jpeg", plot = plot_onlyhull_nmds8_9)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_10_11.jpeg", plot = plot_withhull_nmds10_11)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt10_11.jpeg", plot = plot_wouthull_nmds10_11)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt10_11.jpeg", plot = plot_onlyhull_nmds10_11)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_15_16.jpeg", plot = plot_withhull_nmds15_16)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt15_16.jpeg", plot = plot_wouthull_nmds15_16)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt15_16.jpeg", plot = plot_onlyhull_nmds15_16)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_17.jpeg", plot = plot_withhull_nmds17)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt17.jpeg", plot = plot_wouthull_nmds17)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt17.jpeg", plot = plot_onlyhull_nmds17)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_18.jpeg", plot = plot_withhull_nmds18)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt18.jpeg", plot = plot_wouthull_nmds18)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt18.jpeg", plot = plot_onlyhull_nmds18)
ggsave(filename = "004_plots/invertebrates/nmds/nmds_river type_19.jpeg", plot = plot_withhull_nmds19)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_woh_rt19.jpeg", plot = plot_wouthull_nmds19)
# ggsave(filename = "004_plots/invertebrates/nmds/nmds_onh_rt19.jpeg", plot = plot_onlyhull_nmds19)
# save plots to file (pdf) 


## -- ## 
if (readline("remove all ") == "yes") rm(list = ls())

