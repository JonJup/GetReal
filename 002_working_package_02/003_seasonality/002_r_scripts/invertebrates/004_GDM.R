### ----------------------- ###
### --- GDM --------------- ### 
### - MARCROINVERTEBRATES - ###
### ----------------------- ###

# Fit a generalized dissimilarity model to Macroinvertebrate data. This will
# show how much of the difference between samples is explained by distance rather than by season. 

# date: 08.09.20
# GetReal WP 2 DIATOMS 


# setup -------------------------------------------------------------------
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

dir_sxs = here("002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables/")
dir_mzb = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")
dir_save_plot  = here("002_working_package_02/003_seasonality/004_plots/invertebrates/gdm/") 
dir_save_model = here("002_working_package_02/003_seasonality/003_results/invertebrates/005_gdms/")


# load and prepare data  --------------------------------------------------
files <- dir(path = file.path(dir_sxs))
# select date 
# files <- files[grep(pattern = "2020-09-10", x = files)]

for (i in seq_along(files)) {
    lv <- files[i]
    obj_name <- lv %>% 
        str_extract("rt.*") %>% 
        str_remove(pattern = "\\.RDS")
    assign(x     = obj_name,
           value = readRDS(file.path(dir_sxs,lv)))
    
}
rm(i, lv, obj_name, files);gc()


## --  join tables -- ## 
loop_over_var <- c("1.1", "1.2","2_3", "4_5", "8_11_15_16", "17", "18", "19")
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

rm(loop_over_var);gc()

# Create distance matrix --------------------------------------------------

## -- distance matrix -- ##
files <- ls()[grepl(x = ls(), pattern = "all$")]

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


## -- site locations -- ## 
data <- readRDS(file.path(dir_mzb, "004_2020-11-03_mzb_data1585_low_impact.RDS"))
data <- data[,c("gr_sample_id", "geom")]
data <- unique(data, by = "gr_sample_id")
data %<>% st_as_sf()
data$x <- st_coordinates(data)[,1] %>%  as.numeric()
data$y <- st_coordinates(data)[,2] %>%  as.numeric()
data %<>% st_drop_geometry()

# Fitting models ----------------------------------------------------------

loop_over_var <- c("1.1", "1.2","2_3", "4_5", "8_11_15_16", "17", "18", "19")
for (i in loop_over_var) {
        loop_data_name  <- paste0("rt",i, "_all") 
        loop_data       <- get(loop_data_name)
        n_col           <- ncol(loop_data) - 1
        env_data        <- loop_data[,1:2]
        env_data        <- data[env_data, on = "gr_sample_id"]
        input_data      <- as.data.frame(loop_data[,c(1,3:n_col), with =F ])
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
        
        p1 <- ggplot(data = splineDat, aes(x = x_Geographic, y = y_Geographic)) + geom_line() + ylab("partial ecological distance") + xlab("Geographic Distance") + ylim(0, y_max) + theme_minimal_hgrid() + ggtitle(paste0("Macroinvertebrates - River Type ", i))
        p2 <- ggplot(data = splineDat, aes(x = x_season, y = y_season)) + geom_line() + ylab("partial ecological distance") + xlab("Season") + ylim(0, y_max) + theme_minimal_hgrid() 
        
        p3 <- plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
        
        ggsave(plot = p3, filename = file.path(dir_save_plot,
                                               paste0(Sys.Date(), "_gdm_plot_rt_", i, ".jpeg")))
        
        saveRDS(fit_model, file.path(dir_save_model, paste0(Sys.Date(), "_gdm_rt", i, ".RDS")))
        
        rm(loop_data, loop_data_name, i, n_col, env_data, input_data, gdmTab, fit_model, splineDat, splineDat_x, splineDat_y,max_geo, max_season,y_max, p1,p2,p3);gc()
        }
        


