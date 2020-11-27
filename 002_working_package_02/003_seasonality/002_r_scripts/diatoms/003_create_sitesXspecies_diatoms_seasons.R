# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Diatoms- ------------------------ ### 
### --- Seasonal changes   -------------- ### 
# ----------------------------------------- #

# date written/modified: 02.09.20 + 15.
# date used: 02.09.20 + 08. + 15. + 09.11 + 20. + 23. + 26
# Jonathan Jupke 
# Get Real WP 2
# Diatoms 
# Seasonal changes

# SETUP  --------------------------------------------------------------

## LIBRARIES
pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               here,
               magrittr,
               stringr,
               sf
               )
## DIRECTORES 
DIR = list(dia  = here("002_working_package_02/003_seasonality/003_results/diatoms/"), 
           pd   = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/"),
           save = here("002_working_package_02/003_seasonality/003_results/diatoms/001_speciesXsites_tables/")
)

# LOAD ------------------------------------------------
dt_data  = readRDS(file.path(DIR$dia, "base_data_seasons.RDS"))


# CARPET  --------------------------------------------------------------
dt_data[, c("final_taxon", "gr_sample_id") := .(str_trim(final_taxon, side = "both"),
                                                as.character(gr_sample_id))]

dt_data[final_taxon == "Rossithidium petersennii", final_taxon := "Rossithidium petersenii"]
dt_data[final_taxon == "Adlafia minuscula (Grunow) Lange-Bertalot", final_taxon := "Adlafia minuscula"]

ch_river_types = unique(dt_data$rt)
for (i in seq_along(ch_river_types)){
        if (i == 1) ls_rt = list()    
        ls_rt[[i]] = dt_data[rt == ch_river_types[i]]
        names(ls_rt)[i] = ch_river_types[i]
}

ls_rt$RT1_2_4_sub1 = ls_rt$RT1_2_4[!season %in% c("winter", "spring")]
ls_rt$RT5_sub1     = ls_rt$RT5[data.set %in% c("dia_Naiades", "IRSTEA")]
ls_rt$RT8_sub1     = ls_rt$RT8[data.set != "Janne_Soininen" & !season %in% c("winter", "spring")]
ls_rt$RT9_sub1     = ls_rt$RT9[data.set != "dia_Ecosurv" & !season %in% c("winter", "spring")]
ls_rt$RT16_sub1    = ls_rt$RT16[!season %in% c("spring", "autumn")] 
ls_rt$RT16_sub2    = ls_rt$RT16[!season %in% c("spring", "winter")] 

bbox_rt16 = c("site_00793_date_00984_dia_Naiades", "site_00855_date_00988_dia_Naiades","site_00725_date_00993_dia_Naiades","site_00738_date_00983_dia_Naiades")
bbox_rt16 = filter(ls_rt$RT16_sub1, gr_sample_id %in% bbox_rt16) %>% st_as_sf %>% st_bbox()
ls_rt$RT16_sub1 %<>% st_as_sf() %>%  st_crop(bbox_rt16)
setDT(ls_rt$RT16_sub1)

# subset columns
ls_rt %<>% lapply(function(x)x[,.(gr_sample_id, final_taxon, final_taxon_level, season)] )

ls_spe = lapply(ls_rt, function(x)x[final_taxon_level == "species"])
ls_gen = lapply(ls_rt, function(x)x[final_taxon_level == "genus"])

# sXs --------------------------------------------------------
for (i in seq_along(ls_rt)) {
        
        ld_spe = ls_spe[[i]]
        ld_gen = ls_gen[[i]]
        
        ld_spe[, final_taxon_level := NULL]
        ld_gen[, final_taxon_level := NULL]
        
        # extra table with id and season
        ldj_spe <- copy(ld_spe)
        ldj_gen <- copy(ld_gen)
        ldj_spe[, final_taxon  := NULL] 
        ldj_gen[, final_taxon  := NULL]
        ldj_spe %<>% unique(by = "gr_sample_id")
        ldj_gen %<>% unique(by = "gr_sample_id")
        
        spe_sxs =  splist2presabs(data = ld_spe, sites.col = 1, sp.col = 2) %>% setDT
        gen_sxs =  splist2presabs(data = ld_gen, sites.col = 1, sp.col = 2) %>% setDT
        
        ls_spe[[i]] <- ldj_spe[spe_sxs, on = "gr_sample_id"]
        ls_gen[[i]] <- ldj_gen[gen_sxs, on = "gr_sample_id"]
        
        rm(ld_spe,ld_gen,ldj_spe,i,ldj_gen,spe_sxs,gen_sxs);gc()

}

# Remove rare taxa --------------------------------------------------------

# CO 26.11: instead I use an additional B threshold for A criterion 
# rare taxa are taxa that occur in less that 1% of sampling sites. 

# ls_rare = readRDS(file.path(DIR$pd, "008_a_rare_names.RDS"))
# 
# for (i in seq_along(ls_RT)) {
#         dt_loop = ls_spe[[i]]
#         ch_names_fixed = str_replace_all(
#                 string = names(dt_loop),
#                 pattern = "\\.",
#                 replacement = " "
#         )
#         
#         in_loop_id = which(ch_names_fixed %in% ls_rare[[1]])
#         
#         dt_loop = dt_loop[, -in_loop_id, with = FALSE]
#         ls_spe[[i]] = dt_loop
#         rm(i, dt_loop, ch_names_fixed, in_loop_id)
# }
# 
# rm(ls_rare);gc()

# Are there any entires with only two columns i.e. no taxa ?
for (i in seq_along(ls_rt)) {
        dt_spe = ls_spe[[i]]
        dt_gen = ls_gen[[i]]
        
        if (ncol(dt_spe) < 3) print("species in", names(ls_RT)[i])
        if (ncol(dt_gen) < 3) print("genera in", names(ls_RT)[i])

        rm(dt_spe,dt_gen,i);gc()
}

# Save data to file ---------------------------------------------------

# CO 26.11: instead of saving many files I will combine them in a list and save that. 
# for (i in seq_along(ls_RT)) {
#         
#         dt_save_spe = ls_spe[[i]]
#         dt_save_gen = ls_gen[[i]]
#         ch_save_spe = paste0(names(ls_RT)[i],"_species.RDS")
#         ch_save_gen = paste0(names(ls_RT)[i],"_genus.RDS")
#         saveRDS(object = dt_save_spe,
#                 file = file.path(DIR$save, ch_save_spe))
#         saveRDS(object = dt_save_gen,
#                 file = file.path(DIR$save, ch_save_gen))
# } 

ls_save = list(ls_spe, ls_gen)
saveRDS(file = file.path(DIR$save, "sxs_list.RDS"), ls_save)

## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
