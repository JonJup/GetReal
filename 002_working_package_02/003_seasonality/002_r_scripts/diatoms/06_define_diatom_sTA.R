# --------------------------------------- #
### --- Typical assemblages Diatoms --- ###
### --- Seasonality ------------------- ### 
# --------------------------------------- #
 
# date written\modified : 09.09.20
# date used: 09.09.20; 16.11.20 + 26.11
# Jonathan Jupke
# GetReal WP2
# Seasonality Diatoms  

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, magrittr, dplyr, indicspecies, stringr, here)

## DIRECTORIES
DIR <- list(
        re   = here(
                "002_working_package_02/003_seasonality/003_results/diatoms/"
        )
)
# LOAD ---------------------------------------------------------------
ls_dia = readRDS(file.path(DIR$re, "sxs_list.RDS"))
dt_dia = readRDS(file.path(DIR$re, "base_data_seasons.RDS"))
ls_dia$join = list()

# CARPET ------------------------------------------------------------------

ch_species_names = dt_dia[final_taxon_level == "species", unique(final_taxon)]
ch_genus_names = dt_dia[final_taxon_level == "genus", unique(final_taxon)]

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

# compute indval ----------------------------------------------------------

ls_dia2 = list("sixteen" = ls_dia$join$RT16_sub1, "seventeen" = ls_dia$join$RT17_18)
ls_dia2$sixteen$season.x %<>% droplevels 
ls_dia2$seventeen$season.x %<>% droplevels 

# increased number of permutations to get smaller p-values 
ls_indval = vector("list", length = 2)
ls_indval[[1]] = vector("list", length = 4)
ls_indval[[2]] = vector("list", length = 4)

for (k in seq_along(ls_dia2)) {
        print(paste("K", k, "start @", Sys.time()))
        loop_data   <- ls_dia2[[k]]
        loop_data   <- loop_data[!is.na(season.x)]
        seasons_var <- unique(loop_data$season.x)
        for (l in seq_along(seasons_var)) {
                ls_indval[[k]][[l]] = indicators(
                        X = loop_data[,-c(1:2)],
                        cluster = loop_data$season.x,
                        group = seasons_var[l],
                        max.order = 1,
                        verbose = FALSE,
                        At = 0,
                        Bt = 0,
                        func = "IndVal.g",
                        control = how(nperm = 1)
                )
                names(ls_indval[[k]])[l] = as.character(seasons_var[l])
                print(paste("l", l, "end @", Sys.time()))
        }
        
        
        print(paste("K", k, "end @", Sys.time()))
}
ls_joind = list()
for (k in seq_along(ls_indval)) {
        for (i in seq_along(ls_indval[[k]])) {
                if (i == 1) loop_list = list()
                fl = ls_indval[[k]][[i]]
                if (is.null(fl)) next()
                
                fl1 <- data.table( taxon   = row.names(fl[[7]]), 
                                           A         = fl[[9]],
                                           B         = fl[[10]], 
                                           sqrtIV    = fl[[11]],
                                           p_value   = fl[[12]])
                
                fl1[, season := names(ls_indval[[k]])[i]]
                loop_list[[i]] = fl1
                rm(fl1, fl, i)
        }
        ls_joind[[k]] = rbindlist(loop_list)
        rm(loop_list, k)
}

ch_species_names %<>% str_replace_all(pattern = "\\ ", "\\.")
ch_genus_names %<>% str_replace_all(pattern = "\\ ", "\\.")

# split taxa level 
ls_final = list(
        "spe_16" = ls_joind[[1]][taxon %in% ch_species_names],
        "gen_16" = ls_joind[[1]][taxon %in% ch_genus_names],
        "spe_17_18" = ls_joind[[2]][taxon %in% ch_species_names],
        "gen_17_18" = ls_joind[[2]][taxon %in% ch_genus_names]
)



# SAVE ------------------------------------------------------------

saveRDS(ls_final, file.path(DIR$re, "sta_list.RDS"))

if (readline("delete all? ") == "yes") rm(list = ls())
