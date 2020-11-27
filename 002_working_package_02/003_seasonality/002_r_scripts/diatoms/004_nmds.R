# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Diatoms --------------------- ### 
# ------------------------------------- #

# date: 02.09.20 + 7. + 8. + 15. + 16. + 09.11. + 09.11 + 12.11 + 23.11
# GetReal 
# Working Package 2 
# Diatoms 
# Seasonality 

# Compute and plot NMDS of seasonal diatom communities 

## -- OUTPUT 
# -> 

# SETUP -------------------------------------------------------------------
pacman::p_load(
               data.table, 
               dplyr, 
               fuzzySim, 
               ggplot2, 
               here, 
               magrittr, 
               stringr, 
               tictoc,
               vegan
)

## COLOR PALETTE
ch_colors <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

## DIRECTORIES
DIR <- list(
    re   = here(
        "002_working_package_02/003_seasonality/003_results/diatoms/"
    ),
    save = here(
        "002_working_package_02/003_seasonality/004_plots/diatoms/nmds/"
    )
)
## OPTIONS 
OP <- list(
    COMPUTE_NMDS = FALSE ,
    COMPUTE_DISTANCE = FALSE ,
    LOAD_DISTANCE = TRUE ,
    LOAD_NMDS = TRUE,
    SAVE_PLOTS = TRUE
)
# LOAD ---------------------------------------------------------------
ls_dia = readRDS(file.path(DIR$re, "sxs_list.RDS"))
ls_dia$join = list()

# CARPET ------------------------------------------------------------------
for (i in seq_along(ls_dia[[1]])) {
    lo_spe = ls_dia[[1]][[i]]
    lo_gen = ls_dia[[2]][[i]]
    ls_dia$join[[i]] = left_join(lo_gen,
                                 lo_spe,
                                by = "gr_sample_id") %>%
            setDT
        if (any(duplicated(ls_dia$join[[i]]$gr_sample_id))) {
            print(paste(i, "after 1"))
        }
    names(ls_dia$join)[i] = names(ls_dia[[1]])[i]
    rm(lo_spe, lo_gen, i)
    gc()
}
# check if any NAs in season column remain
for (i in seq_along(ls_dia$join)){
    if (i == 1) seas_na = c()
    seas_na[i] = nrow(ls_dia$join[[i]][is.na(season.x)])
    if (i == length(ls_dia$join)) {
        print(paste("Quality Check", ifelse(sum(seas_na) == 0, "passed", "failed")))
        rm(i, seas_na)
    }
}

# remove i.season column that was created during the join and fix NAs
for (i in seq_along(ls_dia$join)) {
    ls_dia$join[[i]][, season.y := NULL]
    for (j in seq_len(ncol(ls_dia$join[[i]]))) {
        set(ls_dia$join[[i]], which(is.na(ls_dia$join[[i]][[j]])), j, 0)
    }
    rm(i, j)
    gc()
}

# COMPUTE DISTANCE --------------------------------------------------

# loop to create distance matrices 
ls_season = list()
ls_dia$distance = list()
if (OP$COMPUTE_DISTANCE) {
    for (i in seq_along(ls_dia$join)) {
        
        ld                   = t(ls_dia$join[[i]])
        colnames(ld)         = ld[1, ]
        ld                   = ld[-1, ]
        ls_season[[i]]       = ld[1, ]
        names(ls_season)[i]  = names(ls_dia$join)[i]
        ld                   = ld[-1, ]
        taxa_names           = rownames(ld)
        ld                   = apply(ld, 2, as.numeric)
        rownames(ld)         = taxa_names
        ls_dia$distance[[i]] = 1 - simMat(ld, method = "Jaccard")
        names(ls_dia$distance)[i]  = names(ls_dia$join)[i]
        print(i)
        rm(ld, i)
        gc()
        
    }
    saveRDS(file = file.path(DIR$re, "distance_list.RDS"), object = ls_dia$distance)
    saveRDS(file = file.path(DIR$re, "season_list.RDS"), object = ls_season)
}


# LOAD DISTANCE -----------------------------------------------------------
if (OP$LOAD_DISTANCE) {
        ls_dia$distance = readRDS(file = file.path(DIR$re, "distance_list.RDS"))
        ls_season = readRDS(file = file.path(DIR$re, "season_list.RDS"))
}

# COMPUTE NMDS ------------------------------------------------------------

if (OP$COMPUTE_NMDS) {
    ls_dia$nmds = list()
    #for (i in seq_along(ls_dia$distance)) {
    for (i in 4) {
        
        # skip already finished ones 
        # if (i == 1) next()
        ch_save_name = names(ls_dia$join)[i]
        converged = FALSE
        iter = 0
        #while (converged == FALSE & iter < 1000) {
        while (converged == FALSE & iter < 10000) {
            #iter = iter + 500
            iter = iter + 1000
            print(paste(iter, "for", i))
            
            #if (iter == 500) {
            if (iter == 1000) {
                
                loop_nmds  = metaMDS(comm = ls_dia$distance[[i]],
                                     try = iter,
                                     k = 2,
                                     parallel = 6)
            } else {
                loop_nmds = metaMDS(
                    comm = ls_dia$distance[[i]],
                    try = iter,
                    k = 2,
                    previous.best = loop_nmds,
                    parallel = 6
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
        ls_dia$nmds[[i]] = loop_nmds
        names(ls_dia$nmds)[i] = names(ls_dia$join)[i]
        rm(i, ch_save_name, loop_nmds)
    }
}

# LOAD NMDS ---------------------------------------------------------------
if(OP$LOAD_NMDS){
    ch_nmds_files = dir(path = file.path(DIR$re, "nmds/"))
    ls_dia$nmds = list()
    for (i in ch_nmds_files) ls_dia$nmds[[i]] = readRDS(file.path(DIR$re, "nmds/", i))
}

names(ls_dia$nmds) %<>% 
    str_remove("iter[0-9]*") %>% 
    str_remove("__") %>% 
    str_remove("not") %>% 
    str_remove("converged.RDS") %>% 
    str_remove("_$")
    


# PLOT  ----------------------------------------------------------
## prepare data ----
for (i in seq_along(ls_dia$nmds)) {
    if (i == 1) ls_dia$dt_plot = ls_dia$dt_hull = list()
    
    in_seas_id = which(names(ls_season) == names(ls_dia$nmds)[i])
    
    ls_dia$dt_plot[[i]] = 
        data.table(NMDS1 = scores(ls_dia$nmds[[i]])[,1],  
                   NMDS2 = scores(ls_dia$nmds[[i]])[,2], 
                   season = factor(ls_season[[in_seas_id]], 
                                   levels = c("spring", "summer", "autumn", "winter")
                                   )
                   )
    # hulls 
    ls_dia$dt_hull[[i]] = 
        ls_dia$dt_plot[[i]]  %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
    
    rm(in_seas_id, i)
    gc()
}

## plot ----
for (i in seq_along(ls_dia$nmds)) {
    if (i == 1) ls_dia$gg = list()
    
    id = which(names(ls_dia$join) == names(ls_dia$nmds)[i])
    
    ls_dia$gg[[i]] = 
        ggplot(data = ls_dia$dt_plot[[i]],
               aes(x = NMDS1, y = NMDS2)) +
        geom_polygon(data = ls_dia$dt_hull[[i]],   
                     alpha = 0.5, 
                     aes(fill = season)) +
        geom_point(aes(fill = season), 
                   shape = 21) +
        ggtitle(paste0("NMDS", " ", names(ls_dia$join)[id])) +
        labs(fil = "Season",
             subtitle = paste0("Diatoms, Stress: ", round(ls_dia$nmds[[i]]$stress,   2))) +
        scale_fill_manual(values = ch_colors[c(1, 2, 4, 6)])
    
    
}

if (OP$SAVE_PLOTS){
    for (i in seq_along(ls_dia$gg)) {
        
        id = which(names(ls_dia$join) == names(ls_dia$nmds)[i])
        
        ggsave(filename = file.path(DIR$save,
                                    paste0(names(ls_dia$join)[id],
                                           ".png")),
               plot = ls_dia$gg[[i]])
    }
        
}   
## -- ##
if (readline("remove all ") == "yes")
    rm(list = ls())

