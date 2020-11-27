# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Macroinvertebrates ---------- ### 
# ------------------------------------- #

# date: 01.09.20 + 10.09 + 11.09 + 12.11 + 26.11
# GetReal 
# Working Package 2 
# Macroinvertebrates

# SETUP -------------------------------------------------------------------

pacman::p_load(data.table, 
               dplyr, 
               fuzzySim, 
               ggplot2, 
               here, 
               stringr, 
               vegan)
## DIRECTORIES
DIR = list (
    ss = here("002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables/"),
    re = here("002_working_package_02/003_seasonality/003_results/invertebrates/"),
    save = here("002_working_package_02/003_seasonality/004_plots/invertebrates/nmds/") 
)

## OPTIONS 
OPTIONS = list(
    COMPUTE_NMDS = FALSE ,
    COMPUTE_DISTANCE = FALSE ,
    LOAD_DISTANCE = TRUE ,
    LOAD_NMDS = TRUE,
    SAVE_PLOTS = TRUE
)
## COLORS 
ch_col = c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

# LOAD ---------------------------------------------------------------
ls_mzb = readRDS(file.path(DIR$re, "sxs_list.RDS"))
ls_mzb$join = list()

# CARPET ------------------------------------------------------------------
for (i in seq_along(ls_mzb[[1]])) {
    lo_spe = ls_mzb[[1]][[i]]
    lo_gen = ls_mzb[[2]][[i]]
    lo_foh = ls_mzb[[3]][[i]]
    
    ls_mzb$join[[i]] = left_join(lo_gen,
                                 lo_spe,
                                 by = "gr_sample_id") 
    ls_mzb$join[[i]] = left_join(lo_foh,
                                 ls_mzb$join[[i]],
                                 by = "gr_sample_id") %>% 
        setDT
    if (any(duplicated(ls_mzb$join[[i]]$gr_sample_id))) {
        print(paste(i, "after 1"))
    }
    names(ls_mzb$join)[i] = names(ls_mzb[[1]])[i]
    rm(lo_spe, lo_gen, lo_foh, i)
    gc()
}


# check if any NAs in season column remain
for (i in seq_along(ls_mzb$join)){
    if (i == 1) seas_na = c()
    seas_na[i] = nrow(ls_mzb$join[[i]][is.na(season)])
    if (i == length(ls_mzb$join)) {
        print(paste("Quality Check", ifelse(sum(seas_na) == 0, "passed", "failed")))
        rm(i, seas_na)
    }
}

# remove i.season column that was created during the join and fix NAs
for (i in seq_along(ls_mzb$join)) {
    ls_mzb$join[[i]][, c("season.x", "season.y") := NULL]
    for (j in seq_len(ncol(ls_mzb$join[[i]]))) {
        set(ls_mzb$join[[i]], which(is.na(ls_mzb$join[[i]][[j]])), j, 0)
    }
    rm(i, j)
    gc()
}

# COMPUTE DISTANCE --------------------------------------------------

# loop to create distance matrices 
ls_season = list()
ls_mzb$distance = list()
if (OPTIONS$COMPUTE_DISTANCE) {
    for (i in seq_along(ls_mzb$join)) {
        
        ld                   = t(ls_mzb$join[[i]])
        colnames(ld)         = ld[1, ]
        ld                   = ld[-1, ]
        ls_season[[i]]       = ld[1, ]
        names(ls_season)[i]  = names(ls_mzb$join)[i]
        ld                   = ld[-1, ]
        taxa_names           = rownames(ld)
        ld                   = apply(ld, 2, as.numeric)
        rownames(ld)         = taxa_names
        ls_mzb$distance[[i]] = 1 - simMat(ld, method = "Jaccard")
        names(ls_mzb$distance)[i]  = names(ls_mzb$join)[i]
        print(i)
        rm(ld, i)
        gc()
        
    }
    saveRDS(file = file.path(DIR$re, "distance_list.RDS"), object = ls_mzb$distance)
    saveRDS(file = file.path(DIR$re, "season_list.RDS"), object = ls_season)
}


# LOAD DISTANCE -----------------------------------------------------------
if (OPTIONS$LOAD_DISTANCE) {
    ls_mzb$distance = readRDS(file = file.path(DIR$re, "distance_list.RDS"))
    ls_season = readRDS(file = file.path(DIR$re, "season_list.RDS"))
}

# COMPUTE NMDS ------------------------------------------------------------
names(ls_mzb$join)
if (OPTIONS$COMPUTE_NMDS) {
    ls_mzb$nmds = list()
    #for (i in seq_along(ls_mzb$distance)) {
    for (i in 1) {
        
        # skip already finished ones 
        # if (i == 1) next()
        ch_save_name = names(ls_mzb$join)[i]
        converged = FALSE
        iter = 0
        #while (converged == FALSE & iter < 1000) {
        while (converged == FALSE & iter < 10000) {
            #iter = iter + 500
            iter = iter + 1000
            print(paste(iter, "for", i))
            
            #if (iter == 500) {
            if (iter == 1000) {
                
                loop_nmds  = metaMDS(comm = ls_mzb$distance[[i]],
                                     try = iter,
                                     k = 2,
                                     parallel = 2)
            } else {
                loop_nmds = metaMDS(
                    comm = ls_mzb$distance[[i]],
                    try = iter,
                    k = 2,
                    previous.best = loop_nmds,
                    parallel = 2
                )
            }
            
            converged = loop_nmds$converged
        }
        
        ch_save_name = ifelse(
            loop_nmds$converged,
            paste0(ch_save_name, "_iter", iter, "_converged"),
            paste0(ch_save_name, "_iter", iter, "_not_converged")
        )
        
        saveRDS(object = loop_nmds,
                file = file.path(
                    DIR$re,
                    "nmds/",
                    paste0(ch_save_name, ".RDS")
                ))
        ls_mzb$nmds[[i]] = loop_nmds
        names( ls_mzb$nmds)[i] = names(ls_mzb$join)[i]
        rm(i, ch_save_name, loop_nmds)
    }
}


# LOAD NMDS ---------------------------------------------------------------
if(OPTIONS$LOAD_NMDS){
    ch_nmds_files = dir(path = file.path(DIR$re, "nmds/"))
    ls_mzb$nmds = list()
    for (i in ch_nmds_files) ls_mzb$nmds[[i]] = readRDS(file.path(DIR$re, "nmds/", i))
}

names(ls_mzb$nmds) %<>% 
    str_remove("iter[0-9]*") %>% 
    str_remove("__") %>% 
    str_remove("not") %>% 
    str_remove("converged.RDS") %>% 
    str_remove("_$")

# PLOT  ----------------------------------------------------------
## prepare data ----
for (i in seq_along(ls_mzb$nmds)) {
    if (i == 1) ls_mzb$dt_plot = ls_mzb$dt_hull = list()
    
    in_seas_id = which(names(ls_season) == names(ls_mzb$nmds)[i])
    
    ls_mzb$dt_plot[[i]] = 
        data.table(NMDS1 = scores(ls_mzb$nmds[[i]])[,1],  
                   NMDS2 = scores(ls_mzb$nmds[[i]])[,2], 
                   season = factor(ls_season[[in_seas_id]], 
                                   levels = c("spring", "summer", "autumn", "winter")
                   )
        )
    # hulls 
    ls_mzb$dt_hull[[i]] = 
        ls_mzb$dt_plot[[i]]  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
    
    rm(in_seas_id, i)
    gc()
}

## plot ----
for (i in seq_along(ls_mzb$nmds)) {
    if (i == 1) ls_mzb$gg = list()
    
    id = which(names(ls_mzb$join) == names(ls_mzb$nmds)[i])
    
    ls_mzb$gg[[i]] = 
        ggplot(data = ls_mzb$dt_plot[[i]],
               aes(x = NMDS1, y = NMDS2)) +
        geom_polygon(data = ls_mzb$dt_hull[[i]],   
                     alpha = 0.5, 
                     aes(fill = season)) +
        geom_point(aes(fill = season), 
                   shape = 21) +
        ggtitle(paste0("NMDS", " ", names(ls_mzb$join)[id])) +
        labs(fil = "Season",
             subtitle = paste0("Macroinvertebrates, Stress: ", round(ls_mzb$nmds[[i]]$stress,   2))) +
        scale_fill_manual(values = ch_col[c(1, 2, 4, 6)])
    
    
}

if (OPTIONS$SAVE_PLOTS){
    for (i in seq_along(ls_mzb$gg)) {
        
        id = which(names(ls_mzb$join) == names(ls_mzb$nmds)[i])
        
        ggsave(filename = file.path(DIR$save,
                                    paste0(names(ls_mzb$join)[id],
                                           ".png")),
               plot = ls_mzb$gg[[i]])
    }
    
}   
## -- ##
if (readline("remove all ") == "yes")
    rm(list = ls())



# OLD  --------------------------------------------------------------------


files = dir(path = file.path(dir_ss))

for (i in seq_along(files)) {
    lv       = files[i]
    obj_name = lv %>%
        str_extract("rt.*") %>%
        str_remove(pattern = "\\.RDS")
    assign(x     = obj_name,
           value = readRDS(file.path(dir_ss, lv)))
    
}
rm(i, lv, obj_name, files);gc()
file_l <- ls()[grepl(pattern="^rt", x = ls())]
# join tables 
loop_over_var <- c("1.1", "1.2","2_3", "4_5", "8_11_15_16", "17", "18", "19")
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
files <- ls()[grepl(pattern="^rt", x = ls())]

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
if (COMPUTE_DISTANCE){
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
        saveRDS(object = dld,         file = file.path(dir_re, "004_nmds/distance_matrices", paste0("d_", i, ".RDS")))
        saveRDS(object = seasons_var, file = file.path(dir_re, "004_nmds/distance_matrices", paste0("season_",i,".RDS")))
        print(i)
        rm(ld, dld,i, seasons_var);gc()
        
    }
}

if (LOAD_DISTANCE){
    ch_distance_files = dir(path = file.path(dir_re, "004_nmds/distance_matrices/"))
    ch_season_files   = ch_distance_files[str_detect(string=ch_distance_files, pattern = "season")] 
    ch_distance_files = ch_distance_files[!ch_distance_files %in% ch_season_files]
    for (i in seq_along(ch_distance_files)){
        loop_rt    = str_extract(ch_distance_files[i], pattern = "rt.*") %>% 
            str_remove(".RDS")
        
        loop_file1 = readRDS(file.path(dir_re, "004_nmds/distance_matrices", ch_distance_files[i]))
        loop_file2 = readRDS(file.path(dir_re, "004_nmds/distance_matrices", ch_season_files[i]))
        
        assign(x = paste0("d_", loop_rt),value = loop_file1)
        assign(x = paste0("season_", loop_rt),value = loop_file2)
    }
}


files = ls()[grep(pattern="d_rt", x = ls())]

if (COMPUTE_NMDS){
    for (i in files[7:8]){
        loop_data = get(i)
        ch_save_name = str_extract(string = i, pattern = "rt.*") %>% str_remove(pattern="_all")
        ch_save_name = paste0("nmds_", ch_save_name)
        converged = FALSE 
        iter = 0
        while (converged == FALSE & iter < 3000) {
            iter = iter + 500
            print(paste(iter, "for", i))
            
            if (iter == 500) {
                loop_nmds  = metaMDS(comm = loop_data,  try = iter, k = 2)
            } else {
                loop_nmds = metaMDS(comm = loop_data,  try = iter, k = 2, previous.best = loop_nmds)   
            }
            
            converged = loop_nmds$converged
        }
        
        ch_save_name = ifelse(loop_nmds$converged,
                              paste0(ch_save_name, "_iter",iter,"_converged"),
                              paste0(ch_save_name, "_iter",iter,"_not_converged")
        )
        assign(x = ch_save_name,
               value = loop_nmds)
        saveRDS(object = loop_nmds,
                file = file.path(
                    dir_re,
                    "004_nmds/",
                    paste0(Sys.Date(), "_", ch_save_name, ".RDS")
                ))
        rm(i, loop_data, ch_save_name, loop_nmds)
    }
}


## -- read file in 
if (LOAD_NMDS){
    ch_nmds_files     = dir(path = file.path(dir_re, "004_nmds/"), pattern = ".RDS")
    
    for (i in seq_along(ch_nmds_files)){
        loop_rt    = str_extract(ch_nmds_files[i], pattern = "rt.*") %>% 
            str_remove(".RDS")
        
        loop_file1 = readRDS(file.path(dir_re, "004_nmds", ch_nmds_files[i]))
        
        assign(x = paste0(loop_rt),value = loop_file1)
    
    }
}

files = ls()[grepl(pattern = "iter", x = ls())]
files2 = ls()[grepl(pattern = "season", x = ls())]
files2 = files2[files2 != "ch_season_files"]
# prepare data for plots ----------------------------------------------------------
for (i in seq_along(files)) {
    loop_data = files[i]
    loop_data = get(loop_data)
    season_data = files2[i]
    season_data = get(season_data)
    loop_rt   = str_extract(files[i], "rt.*") %>% str_remove("_iter.*")
    loop_dt   = data.table(NMDS1 = scores(loop_data)[,1]  ,  NMDS2 = scores(loop_data)[,2]  , season = factor(season_data, levels = c("spring", "summer", "autumn", "winter")))
    assign(x = paste0("dt_nmds_", loop_rt), value = loop_dt)
}

files = ls()[grepl(pattern = "dt_nmds", x = ls())]
### ---  hulls --- ###  
for (i in seq_along(files)) {
    loop_data = files[i]
    loop_data = get(loop_data)
    loop_rt   = str_extract(files[i], "rt.*") %>% str_remove("_nmds.*")
    loop_dt   = loop_data  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
    assign(x = paste0("hull_", loop_rt), value = loop_dt)
}
files_hull = ls()[grepl(pattern = "hull", x = ls())]
files_nmds  = ls()[grepl(patter  = "iter", x = ls())]

for (i in seq_along(files)) {
    loop_data = files[i]
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
}


files_plots = ls()[grepl(pattern= "gg_", x = ls())]

if (SAVE_PLOTS){
    for (i in seq_along(files_plots)) {
        
        loop_plot = get(files_plots[i])
        loop_rt   = str_extract(files_plots[i], "rt.*") %>% str_remove("_nmds.*")
        ch_save_name = paste0(Sys.Date(), "_nmds_plot_", loop_rt, ".png")
        ggsave(filename = file.path(dir_save, ch_save_name),
               plot = loop_plot)
    }

## -- ## 
if (readline("remove all ") == "yes") rm(list = ls())
}
