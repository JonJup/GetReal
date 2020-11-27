### ----------------------- ###
### --- GDM --------------- ### 
### - DIATOMS ----------- - ###
### ----------------------- ###

# Fit a generalized dissimilarity model to Macroinvertebrate data. This will
# show how much of the difference between samples is explained by distance rather than by season. 

# date: 08.09.20 + 16. + 27.11.20
# GetReal 
# Working Package 2 
# DIATOMS 

# SETUP -------------------------------------------------------------------
pacman::p_load(cowplot, 
               data.table,
               dplyr,
               fuzzySim,
               gdm, 
               ggplot2,
               here,
               magrittr,
               sf,
               stringr
               
)
DIR = list(
    re = here(
        "002_working_package_02/003_seasonality/003_results/diatoms/"
    ),
    dia = here(
        "002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/"
    ),
    save_plot  = here(
        "002_working_package_02/003_seasonality/004_plots/diatoms/gdm/"
    ) ,
    save_model = here(
        "002_working_package_02/003_seasonality/003_results/diatoms/gdm/"
    )
)

# LOAD  --------------------------------------------------
ls_data  = readRDS(file.path(DIR$re,  "sxs_list.RDS"))
dt_sites = readRDS(file.path(DIR$re, "001_2020-11-09_diatoms_w_rt.RDS"))

# CARPET ------------------------------------------------------------------
## bio data ----------------

if (length(ls_data[[1]]) == length(ls_data[[2]])){
    print("quality check passed")
} else {
    print("quality check failed")
}

for (i in seq_along(ls_data[[1]])) {
    
    if (i==1) ls_data$join = list()
    ls_data$join[[i]] <- ls_data[[2]][[i]][ls_data[[1]][[i]], on = "gr_sample_id"]
    if (any(duplicated(ls_data$join[[i]]$gr_sample_id))) {print(paste(i, "after 1"))}
    # fix season column 
    if ("i.season" %in% names(ls_data$join[[i]])){
        ls_data$join[[i]][is.na(season) & !is.na(i.season), season := i.season]
        ls_data$join[[i]][, c("i.season") := NULL]
    }
    # fix NAs with code from Matt Dowle; see: 
    # (https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table)
    for (j in seq_len(ncol(ls_data$join[[i]])))
        set(ls_data$join[[i]], which(is.na(ls_data$join[[i]][[j]])), j, 0)
    
    print(i)
    rm(i, j)
    gc()
    
}

names(ls_data$join) = names(ls_data[[1]])

# quality check - problematic data set number is printed. Best of nothing is returned 
for (i in seq_along(ls_data$join)) {
    n  <- nrow(ls_data$join[[i]][is.na(season)])
    if (n != 0) print(i)
    rm(i)
    gc()
}

## site data ---------------- 

dt_sites = dt_sites[,c("gr_sample_id", "geometry")] %>%  
    unique(by = "gr_sample_id") %>% 
    st_as_sf()
dt_sites$x = st_coordinates(dt_sites)[,1] %>%  as.numeric()
dt_sites$y = st_coordinates(dt_sites)[,2] %>%  as.numeric()
dt_sites %<>% st_drop_geometry()

# FIT MODEL ----------------------------------------------------------
for (i in seq_along(ls_data$join)) {
    #loop_data_name  <- paste0("rt",i, "_all") 
    #loop_data       <- get(loop_data_name)
    n_col           <- ncol(ls_data$join[[i]]) - 1
    env_data        <- ls_data$join[[i]][,1:2]
    env_data        <- dt_sites[env_data, on = "gr_sample_id"]
    input_data      <- as.data.frame(ls_data$join[[i]][, c(1,3:n_col), with =F])
    env_data        %<>% as.data.frame
    env_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter"))
    
    gdmTab <- formatsitepair(bioData = input_data, 
                             bioFormat = 1,
                             dist = "jaccard",
                             siteColumn = "gr_sample_id",
                             predData = env_data,
                             XColumn = "x",
                             YColumn = "y",
                             sampleSites = 1
    )
    fit_model  <- gdm(gdmTab, geo = T)
    splineDat  <- isplineExtract(fit_model)
    splineDat_x <- splineDat$x %>% as.data.frame
    splineDat_y <- splineDat$y %>% as.data.frame
    names(splineDat_x) <- paste0("x_", names(splineDat_x))
    names(splineDat_y) <- paste0("y_", names(splineDat_y))
    splineDat <- cbind(splineDat_x, splineDat_y)
    max_geo    <- max(splineDat$y_Geographic)
    max_season <- max(splineDat$y_season)
    y_max <- max(max_geo, max_season)
    
    loop_name = names(ls_data$join)[i]
    
    p1 <- ggplot(data = splineDat, aes(x = x_Geographic, y = y_Geographic)) + geom_line() + ylab("partial ecological distance") + xlab("Geographic Distance") + ylim(0, y_max) + theme_minimal_hgrid() + ggtitle(paste0("Diatoms -", loop_name))
    p2 <- ggplot(data = splineDat, aes(x = x_season, y = y_season)) + geom_line() + ylab("partial ecological distance") + xlab("Season") + ylim(0, y_max) + theme_minimal_hgrid() 
    
    p3 <- plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
    
    ggsave(plot = p3, filename = file.path(DIR$save_plot,
                                           paste0("gdm_", loop_name, ".jpeg")))
    
    saveRDS(fit_model, file.path(DIR$save_model, paste0("gdm_", loop_name, ".RDS")))
    
    rm(loop_data, loop_data_name, i, n_col, env_data, input_data, gdmTab, fit_model, splineDat, splineDat_x, splineDat_y,max_geo, max_season,y_max, p1,p2,p3);gc()
}



# OLD ---------------------------------------------------------------------
# 
# 
# # setup -------------------------------------------------------------------
# pacman::p_load(cowplot, 
#                data.table,
#                dplyr,
#                fuzzySim,
#                gdm, 
#                ggplot2,
#                here,
#                magrittr,
#                sf,
#                stringr
# )
# setwd(here("002_working_package_02/003_seasonality/"))
# 
# # load and prepare data  --------------------------------------------------
# files <- dir(path = "003_results/diatoms/001_speciesXsites_tables/")
# #files <- files[grep(pattern = "2020-09-08", x = files)]
# 
# for (i in seq_along(files)) {
#         lv <- files[i]
#         obj_name <- lv %>% 
#                 str_extract("rt_.*") %>% 
#             str_remove(".RDS")
#         assign(x     = obj_name,
#                value = readRDS(paste0("003_results/diatoms/001_speciesXsites_tables/",lv)))
#         
# }
# rm(i, lv, obj_name, files);gc()
# file_l <- ls()[grepl(x = ls(), pattern = "rt")]
# 
# ## --  join tables -- ## 
# loop_over_var <- c("10","11","14", "15", "large")
# for (i in loop_over_var) {
#         
#         file_pre <- paste0("rt_", i, "_")
#         files    <- ls()[grepl(pattern = file_pre, x = ls())]
#         ld1 <- get(files[1]) 
#         ld2 <- get(files[2])
#         ldj <- ld2[ld1, on = "gr_sample_id"]
#         if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 1"))}
#         rm(ld1, ld2);gc()
#         
#         assign(x = paste0(file_pre, "all"), 
#                value = ldj)
#         print(i)
#         rm(ldj, files, file_pre, i);gc()
#         
# }
# rm(list = file_l)
# rm(file_l, loop_over_var);gc()
# 
# # Create distance matrix --------------------------------------------------
# 
# files <- ls()
# 
# if (
#     nrow(rt_10_all[is.na(season)]) != 0 |
#     nrow(rt_11_all[is.na(season)]) != 0 |
#     nrow(rt_14_all[is.na(season)]) != 0 |
#     nrow(rt_15_all[is.na(season)]) != 0 |
#     nrow(rt_large_all[is.na(season)]) != 0) {
#         print("Quality Check Failed")
# } else {
#         print("Quality Check Passed")
# }
# 
# # remove i.season column that was created during the join
# for (i in files){
#         ld <- get(i)
#         ld[,i.season := NULL]
#         assign(x = i, 
#                value = ld)
#         rm(ld, i);gc()
# }
# 
# 
# ## -- site locations -- ## 
# dia  <- readRDS("003_results/diatoms/001_2020-11-09_diatoms_w_rt.RDS")
# dia <- dia[,c("gr_sample_id", "geometry")]
# dia <- unique(dia, by = "gr_sample_id")
# dia %<>% st_as_sf()
# dia$x <- st_coordinates(dia)[,1] %>%  as.numeric()
# dia$y <- st_coordinates(dia)[,2] %>%  as.numeric()
# dia %<>% st_drop_geometry()
# # - 
# 
# 
# # Fitting models ----------------------------------------------------------
# 
# 
# loop_over_var <- c("10","11","14", "15", "large")
# for (i in loop_over_var) {
#         loop_data_name  <- paste0("rt_",i, "_all") 
#         loop_data       <- get(loop_data_name)
#         n_col           <- ncol(loop_data) - 1
#         env_data        <- loop_data[,1:2]
#         env_data        <- dia[env_data, on = "gr_sample_id"]
#         input_data      <- as.data.frame(loop_data[,c(1,3:n_col), with =F ])
#         env_data        %<>% as.data.frame
#         env_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter"))
#         
#         gdmTab <- formatsitepair(bioData = input_data, 
#                                  bioFormat = 1,
#                                  dist = "jaccard",
#                                  siteColumn = "gr_sample_id",
#                                  predData = env_data,
#                                  XColumn = "x",
#                                  YColumn = "y",
#                                  sampleSites = 1
#                                  )
#         fit_model <- gdm(gdmTab, geo = T)
#         splineDat  <- isplineExtract(fit_model)
#         splineDat_x <- splineDat$x %>% as.data.frame
#         splineDat_y <- splineDat$y %>% as.data.frame
#         names(splineDat_x) <- paste0("x_", names(splineDat_x))
#         names(splineDat_y) <- paste0("y_", names(splineDat_y))
#         splineDat <- cbind(splineDat_x, splineDat_y)
#         
#         max_geo    <- max(splineDat$y_Geographic)
#         max_season <- max(splineDat$y_season)
#         
#         y_max <- max(max_geo, max_season)
#         
#         p1 <- ggplot(data = splineDat, aes(x = x_Geographic, y = y_Geographic)) + geom_line() + ylab("partial ecological distance") + xlab("Geographic Distance") + ylim(0, y_max) + theme_minimal_hgrid() + ggtitle(paste0("Diatoms - River Type ", i))
#         p2 <- ggplot(data = splineDat, aes(x = x_season, y = y_season)) + geom_line() + ylab("partial ecological distance") + xlab("Season") + ylim(0, y_max) + theme_minimal_hgrid() 
#         
#         p3 <- plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
#         
#         ggsave(plot = p3, filename = paste0("004_plots/diatoms/gdm/", Sys.Date(), "_gdm_plot_rt_", i,".jpeg"))
#         
#         saveRDS(fit_model, paste0("003_results/diatoms/004_gdms/", Sys.Date(), "_gdm_rt", i, ".RDS"))
#         
#         rm(loop_data, loop_data_name, i, n_col, env_data, input_data, gdmTab, fit_model, splineDat, splineDat_x, splineDat_y,max_geo, max_season,y_max, p1,p2,p3);gc()
#         }
#         
# 
# # fit model  --------------------------------------------------------------
# 
# 
# 
# 
# # analyze model results  --------------------------------------------------
# 
# 
