# --------------------------------------- #
### --- Typical assemblages Diatoms --- ### 
# --------------------------------------- #

#date written\modified : 04.11.
#date used: 04.11. 
#Jonathan Jupke
# GetReal
# Working Package 2 
# Diatoms  

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, 
               dplyr, 
               here,
               indicspecies,
               magrittr, 
               stringr)

dir_pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")


# load data  --------------------------------------------------------------
dt_spe = readRDS(file.path(dir_pd, "009_2020-11-04diatom_species_no_rare_w_LSRT.RDS"))
dt_gen = readRDS(file.path(dir_pd, "009_2020-11-04diatom_genus_no_rare_w_LSRT.RDS"))

# join data ---------------------------------------------------------------
ch_spe_names <- names(dt_spe)[-(1:4)]
ch_gen_names <- names(dt_gen)[-(1:4)]

dt_all <- dt_spe[dt_gen, on = "gr_sample_id"]
dt_all[, c("i.ls_bd_20", "i.WSO1_ID", "i.geometry" ) := NULL]

# Remove NAs 
for (j in seq_len(ncol(dt_all))) set(dt_all, which(is.na(dt_all[[j]])), j, 0)

# test that taxa vector will work 
rm(dt_spe, dt_gen, j );gc()


# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps

# Comments: 
# 10 Actually has a lot of sites but they are almost all in France 
#-> Heavy bias 
#-> Could be moved to medium  
# 15 Similar to 10. Many streams do not fall into GR Countries. 
# -> Could be moved to medium

ch_medium_types <- paste0("RT", c(1, 2, 3, 4, 5, 6, 8, 9, 12, 16, 17, 18, 19))
# bad_types    <- paste0("RT", c(7, 10, 11, 15))

# subset data sets according to the visual categorization of stream type representation
dt_all   <- dt_all[ls_bd_20 %in% ch_medium_types]

dt_all[ls_bd_20 %in% c("RT17", "RT4"), ls_bd_20 := "RT4_17"]
dt_all[ls_bd_20 %in% c("RT4_17", "RT2"), ls_bd_20 := "RT2_4_17"]
dt_all[ls_bd_20 %in% c("RT2_4_17", "RT1"), ls_bd_20 := "RT1_2_4_17"]
dt_all[ls_bd_20 %in% c("RT1_2_4_17", "RT18"), ls_bd_20 := "RT1_2_4_17_18"]
dt_all[ls_bd_20 %in% c("RT8", "RT19"), ls_bd_20 := "RT8_19"]
dt_all[ls_bd_20 %in% c("RT1_2_4_17_18", "RT8_19"), ls_bd_20 := "RT_large"]
dt_all[ls_bd_20 %in% c("RT16"), ls_bd_20 := "RT_large"]
dt_all[ls_bd_20 %in% c("RT5"), ls_bd_20 := "RT_large"]

## -- quality check -- ## 
# if (dt_all[ls_bd_20 %in% c("RT17", "RT4", "RT2", "RT1", "RT18", "RT8", "RT19"), .N] != 0 |
#     dt_all[ls_bd_20 %in% c("RT_large"), .N] == 0) {
#         print("quality check failed")
# } else {
#         print("quality check passed")
# }

# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completely NA sign matrix.
# Turn river type into factor. 
dt_all$ls_bd_20 %<>% factor
dt_all$ls_bd_20 %<>% droplevels()
ch_river_types = as.character(unique(dt_all$ls_bd_20))


# compute indval ----------------------------------------------------------

# increased number of permutations to get smaller p-values 

ls_groups = list()

for (i in seq_along(ch_river_types)) {
        
        group_var = ch_river_types[i]
        print(paste(group_var, "started @", format(Sys.time(), format="%H:%M:%S")))
        i_print   = ifelse(i < 10, paste0("0", i), i)
        
        
        # skip if river type is not present in data 
        if (dt_all[ls_bd_20 == group_var, .N] == 0)
                next()

        ls_groups[[i]] = indicators(
                X         = dt_all[, -c(1:4)],
                cluster   = dt_all$ls_bd_20,
                group     = group_var,
                max.order = 1,
                verbose   = FALSE,
                At        = 0,
                Bt        = 0,
                func      = "IndVal.g",
                control   = how(nperm = 1)
        )
        
        names(ls_groups)[i] = group_var
        
        print(paste(group_var, "ended @", format(Sys.time(), format="%H:%M:%S")))
        
        rm(group_var)

}; beepr::beep()

for (i in seq_along(ls_groups)) {
        files_loop  <- ls_groups[[i]]
        files_loop2 <-
                data.table(
                        taxon   = row.names(files_loop[[7]]),
                        A       = files_loop[[9]],
                        B       = files_loop[[10]],
                        sqrtIV  = files_loop[[11]],
                        p_value = files_loop[[12]]
                )
        files_loop2[, group := names(ls_groups)[i]]
        ls_groups[[i]] <- files_loop2
}

dt_groups = rbindlist(ls_groups)

## -- quality check -- ## 
if (dt_groups[group %in% c("RT17", "RT4", "RT2", "RT1", "RT18", "RT8", "RT19"), .N] != 0 |
    dt_groups[group %in% c("RT_large"), .N] == 0) {
        print("quality check failed")
} else {
        print("quality check passed")
}

dt_spe <- dt_groups[taxon %in% ch_spe_names]
dt_gen <- dt_groups[taxon %in% ch_gen_names]
## -- quality check -- ## 
if (nrow(dt_spe) + nrow(dt_gen) != nrow(dt_groups)) {
        print("quality check failed")
} else {
        print("quality check passed")
}


# save to file ------------------------------------------------------------

saveRDS(object = dt_spe, file = file.path(dir_pd, paste0("11_indicator_spe.RDS")))
saveRDS(object = dt_gen, file = file.path(dir_pd, paste0("11_indicator_gen.RDS")))

# empty environment 
if (readline("Delete all? ") == "yes") rm(list = ls())

