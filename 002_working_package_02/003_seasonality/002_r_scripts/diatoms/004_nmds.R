# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Diatoms --------------------- ### 
# ------------------------------------- #

# date: 02.09.20 + 7. + 8. + 15. + 16. + 09.11. + 09.11 + 12.11
# GetReal 
# Working Package 2 
# Diatoms 
# Seasonality 

# Look for differences between seasons using NMDS 


# setup -------------------------------------------------------------------

pacman::p_load(
               data.table, 
               dplyr, 
               fuzzySim, 
               ggplot2, 
               here, 
               magrittr, 
               stringr, 
               vegan
)

## COLOR PALETTE
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")


## DIRECTORIES
dir <- list()
dir$re   = here("002_working_package_02/003_seasonality/003_results/diatoms/")
dir$ss   = here("002_working_package_02/003_seasonality/003_results/diatoms/001_speciesXsites_tables/")
dir$save = here("002_working_package_02/003_seasonality/004_plots/diatoms/nmds/")

## OPTIONS 
OP <- list()
OP$COMPUTE_NMDS = FALSE 
OP$COMPUTE_DISTANCE = FALSE 
OP$LOAD_DISTANCE = TRUE 
OP$LOAD_NMDS = TRUE
OP$SAVE_PLOTS = TRUE

# load and prepare data ---------------------------------------------------------------
ch_files = dir(path = file.path(dir$re, "001_speciesXsites_tables"))
for (i in seq_along(ch_files)) {
        lv = ch_files[i]
        obj_name <- lv %>% 
                str_extract("rt_.*_[a-z]*")
        assign(x     = obj_name,
               value = readRDS(file.path(dir$re, "001_speciesXsites_tables", lv)))
        rm(lv, obj_name, i)
        
}
ch_files <- ls()[grepl(x = ls(), pattern = "rt_")]

## --  join tables -- ## 
loop_over_var <- c("10","11","14", "15","large")
for (i in loop_over_var) {
        
    files    <- ls()[grepl(pattern = paste0("rt_", i, "_"), 
                           x = ls())]
    ld1 <- get(files[1])
    ld2 <- get(files[2])
    ldj <- ld2[ld1, on = "gr_sample_id"]
    if (any(duplicated(ldj$gr_sample_id))) {
        print(paste(i, "after 1"))
    }
    rm(ld1, ld2)
    gc()
    
    assign(x = paste0(paste0("rt_", i, "_"), 
                      "all"),
           value = ldj)
    print(i)
    rm(ldj, files, file_pre, i)
    gc()
        
}
rm(list = ch_files)
rm(ch_files, loop_over_var);gc()


# Create distance matrix --------------------------------------------------

ch_files <- ls()[grepl(x = ls(), pattern = "rt_")]

if (nrow(rt_large_all[is.na(season)]) != 0 |
    nrow(rt_10_all[is.na(season)]) != 0 |
    nrow(rt_11_all[is.na(season)]) != 0 |
    nrow(rt_14_all[is.na(season)]) != 0 |
    nrow(rt_15_all[is.na(season)]) != 0 ) {
        print("Quality Check Failed")
} else {
        print("Quality Check Passed")
}

# remove i.season column that was created during the join
for (i in ch_files){
        ld <- get(i)
        ld[,i.season := NULL]
        assign(x = i, 
               value = ld)
        rm(ld, i);gc()
}

# loop to create distance matrices 
if (OP$COMPUTE_DISTANCE) {
    for (i in ch_files) {
        ld           = get(i)
        ld           = t(ld)
        colnames(ld) = ld[1, ]
        ld           = ld[-1, ]
        seasons_var  = ld[1, ]
        ld           = ld[-1, ]
        taxa_names   = rownames(ld)
        ld           = apply(ld, 2, as.numeric)
        rownames(ld) = taxa_names
        dld          = 1 - simMat(ld, method = "Jaccard")
        assign(x = paste0("d_", i), value = dld)
        assign(x = paste0("season_", i), value = seasons_var)
        saveRDS(object = dld,         file = file.path(dir$re, "003_nmds/distance_matrices", paste0("d_", i, ".RDS")))
        saveRDS(object = seasons_var, file = file.path(dir$re, "003_nmds/distance_matrices", paste0("season_",i,".RDS")))
        print(i)
        rm(ld, dld, i, seasons_var)
        gc()
        
    }
}

# run NMDS ----------------------------------------------------------------

# load distance matrices 
if (OP$LOAD_DISTANCE) {
    for (i in ch_files) {
        distance_name = paste0("d_",  i)
        season_name   = paste0("season_",  i)
        distance_file_name = paste0(distance_name, ".RDS")
        season_file_name   = paste0(season_name, ".RDS")
        assign(x = distance_name,
               value = readRDS(
                   file = file.path(dir$re, "003_nmds/", "distance_matrices/", distance_file_name)
               ))
        assign(x = season_name,
               value = readRDS(
                   file = file.path(dir$re, "003_nmds/", "distance_matrices/", season_file_name)
               ))
    }
    rm(distance_name, season_name, distance_file_name, season_file_name)
}

ch_files = ls()[grep(pattern = "d_rt", x = ls())]

if (OP$COMPUTE_NMDS) {
    for (i in files[5]) {
        loop_data = get(i)
        ch_save_name = str_extract(string = i, pattern = "rt.*") %>% str_remove(pattern =
                                                                                    "_all")
        ch_save_name = paste0("nmds_", ch_save_name)
        converged = FALSE
        iter = 0
        if (i != "d_rt_large_all") {
            while (converged == FALSE & iter < 2000) {
                iter = iter + 500
                print(paste(iter, "for", i))
                
                if (iter == 500) {
                    loop_nmds  = metaMDS(comm = loop_data,
                                         try = iter,
                                         k = 2)
                } else {
                    loop_nmds = metaMDS(
                        comm = loop_data,
                        try = iter,
                        k = 2,
                        previous.best = loop_nmds
                    )
                }
                
                converged = loop_nmds$converged
            }
        } else {
            iter == 500
            loop_nmds  = metaMDS(comm = loop_data,
                                 try = iter,
                                 k = 2)
            converged = loop_nmds$converged
        }
        ch_save_name = ifelse(
            loop_nmds$converged,
            paste0(ch_save_name, "_iter", iter, "_converged"),
            paste0(ch_save_name, "_iter", iter, "_not_converged")
        )
        assign(x = ch_save_name,
               value = loop_nmds)
        saveRDS(object = loop_nmds,
                file = file.path(
                    dir$re,
                    "003_nmds/",
                    paste0(Sys.Date(), "_", ch_save_name, ".RDS")
                ))
        rm(i, loop_data, ch_save_name, loop_nmds)
    }
}

ch_files = dir(file.path(dir$re, "003_nmds"))

if(OP$LOAD_NMDS){
    for (i in ch_files) {
        if (!str_detect(string = i, pattern = "nmds")) next()
        load_name = str_extract(string = i, 
                                pattern = "nmds_rt_[0-9].")
        if (is.na(load_name)) {
            load_name = "nmds_rt_large"
        }
        assign(x = load_name,
               value = readRDS(
                   file = file.path(dir$re, "003_nmds/", i)
               ))
    }
}

# Plots ---------------------------------------------------------------
files = ls()[grepl(pattern = "nmds", x = ls())]
files2 = ls()[grepl(pattern = "season", x = ls())]
files2 = files2[files2 != "season_file_name"]
# prepare data for plots ----------------------------------------------------------
for (i in seq_along(files)) {
    loop_data = files[i]
    loop_data = get(loop_data)
    season_data = files2[i]
    season_data = get(season_data)
    loop_rt   = str_extract(files[i], "rt.*") %>% str_remove("_iter.*")
    loop_dt   = data.table(NMDS1 = scores(loop_data)[,1]  ,  NMDS2 = scores(loop_data)[,2]  , season = factor(season_data, levels = c("spring", "summer", "autumn", "winter")))
    assign(x = paste0("dt_nmds_", loop_rt), value = loop_dt)
    rm(loop_data, season_data, loop_rt, loop_dt)
}

ch_files = ls()[grepl(pattern = "dt_nmds", x = ls())]
### ---  hulls --- ###  
for (i in seq_along(ch_files)) {
    loop_data = ch_files[i]
    loop_data = get(loop_data)
    loop_rt   = str_extract(files[i], "rt.*") %>% str_remove("_nmds.*")
    loop_dt   = loop_data  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
    assign(x = paste0("hull_", loop_rt), value = loop_dt)
    rm(loop_data, loop_rt, loop_dt)
}

files_hull  = ls()[grepl(pattern = "hull", x = ls())]
files_nmds  = ls()[grepl(patter  = "^nmds", x = ls())]

for (i in seq_along(ch_files)) {
    loop_data = ch_files[i]
    loop_data = get(loop_data)
    loop_rt   = str_extract(files[i], "rt.*") %>% str_remove("_nmds.*")
    loop_hull = get(files_hull[i])
    nmds_data = get(files_nmds[i])
    loop_plot = ggplot(data = loop_data,   aes(x = NMDS1, y = NMDS2)) +
        geom_polygon(data = loop_hull,   alpha = 0.5, aes(fill = season)) +
        geom_point(aes(fill = season), shape = 21) +
        ggtitle(paste0( "NMDS"," ", loop_rt)) +
        labs(fil = "Season", subtitle = paste0("Macroinvertebrates, Stress: ", round(nmds_data$stress,   2))) +
        scale_fill_manual(values = my_color_palette[c(1, 2, 4, 6)])
    assign(x = paste0("gg_",loop_rt,"_nmds"),
           value = loop_plot)
    rm(loop_data, loop_rt, nmds_data, loop_plot)
}

files_plots = ls()[grepl(pattern= "gg_", x = ls())]

if (OP$SAVE_PLOTS){
    for (i in seq_along(files_plots)) {
        
        loop_plot = get(files_plots[i])
        loop_rt   = str_extract(files_plots[i], "rt.*") %>% str_remove("_nmds.*")
        ch_save_name = paste0(Sys.Date(), "_nmds_plot_", loop_rt, ".png")
        ggsave(filename = file.path(dir$save, ch_save_name),
               plot = loop_plot)
    }
    
    ## -- ## 
    if (readline("remove all ") == "yes") rm(list = ls())
}
