# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Diatoms- ------------------------ ### 
### --- Seasonal changes   -------------- ### 
# ----------------------------------------- #

# date written/modified: 02.09.20 + 15.
# date used: 02.09.20 + 08. + 15. + 09.11 + 20.
# Jonathan Jupke 
# Get Real WP 2
# Diatoms 
# Seasonal changes

# Setup  --------------------------------------------------------------

# LIBRARIES
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

# Read in and prepare data ------------------------------------------------
dt_data  = readRDS(file.path(DIR$dia, "001_2020-11-09_diatoms_w_rt.RDS"))

dt_data[, final_taxon := str_trim(final_taxon, side = "both")]

# Subset  --------------------------------------------------------------
dt_data$gr_sample_id %<>% as.character()

dt_data <- dt_data[(is.na(year) | year >= 2000) & !is.na(season)]

unique(dt_data$season)

dt_data   <- dt_data[rt %in% paste0("RT", c(1, 2, 3, 4, 5, 6, 8, 9, 12, 16, 17, 18, 19))]

dt_data[rt %in% c("RT1", "RT17", "RT18", "RT19"), rt := "RT1_17_18_19"]
dt_data[rt %in% c("RT2", "RT4"), rt := "RT2_4"]
dt_data[rt %in% c("RT8", "RT9"), rt := "RT8_9"]
dt_data[rt %in% c("RT8_9", "RT2_4"), rt := "RT2_4_8_9"]
dt_data[rt %in% c("RT2_4_8_9", "RT1_17_18_19"), rt := "RT1_2_4_8_9_17_18_19"]

ch_river_types = unique(dt_data$rt)
ls_RT = list()
ls_RT$RT1_2_4_8_9_17_18_19       = dt_data[rt == "RT1_2_4_8_9_17_18_19"]
ls_RT$RT1_2_4_8_9_17_18_19_sub_1 = ls_RT$RT1_2_4_8_9_17_18_19[!season %in% c("winter", "spring")]

st_spatial_subset = filter(
        dt_data,
        gr_sample_id %in% c(
                "site_03147_date_00137_DIA_edwin_peters",
                "site_00332_date_00004_dia_Saul_Blanco",
                "site_00136_date_00252_dia_MiguelIglesias",
                "site_00102_date_00038_dia_Ecosurv"
        )
) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf() %>%
        st_bbox()

ls_RT$RT1_2_4_8_9_17_18_19_sub_2 = ls_RT$RT1_2_4_8_9_17_18_19 %>% 
        st_as_sf() %>% 
        st_crop(st_spatial_subset) %>% 
        setDT

ls_RT$RT3 = dt_data[rt == "RT3"]
ls_RT$RT5 = dt_data[rt == "RT5"]
ls_RT$RT5_sub = ls_RT$RT5[data.set %in% c("dia_Naiades", "IRSTEA")]
ls_RT$RT16 = dt_data[rt == "RT16"] 
ls_RT$RT16_sub1 = ls_RT$RT16[!season %in% c("spring", "autumn")] 

st_spatial_subset = filter(
        dt_data,
        gr_sample_id %in% c("site_00793_date_00984_dia_Naiades", 
                            "site_00855_date_00988_dia_Naiades",
                            "site_00725_date_00993_dia_Naiades",
                            "site_00738_date_00983_dia_Naiades")) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf() %>%
        st_bbox()
ls_RT$RT16_sub1 = ls_RT$RT16_sub1 %>% 
        st_as_sf() %>% 
        st_crop(st_spatial_subset) %>% 
        setDT

ls_RT$RT16_sub2 = ls_RT$RT16[!season %in% c("winter", "spring")]

rm(st_spatial_subset);gc()


# subset columns

for (i in seq_along(ls_RT)) {
        
        ld <- ls_RT[[i]]
        ld <- ld[,.(gr_sample_id, final_taxon, final_taxon_level, season)] 
        ls_RT[[i]] = ld
        rm(ld, i)
}
ls_spe = list() 
ls_gen = list() 

## -- different levels

        
for (i in seq_along(ls_RT)) {
        ls_spe[[i]] = ls_RT[[i]][final_taxon_level == "species"]
        ls_gen[[i]] = ls_RT[[i]][final_taxon_level == "genus"]
}

# Turn to site X species matrix --------------------------------------------------------

 
for (i in seq_along(ls_RT)) {
        
        ld_spe = ls_spe[[i]]
        ld_gen = ls_gen[[i]]
        
        ld_spe[, final_taxon_level := NULL]
        ld_gen[, final_taxon_level := NULL]
        
        ldj_spe <- copy(ld_spe)
        ldj_gen <- copy(ld_gen)
        
        ldj_spe[, final_taxon  := NULL]
        ldj_gen[, final_taxon  := NULL]
        
        ldj_spe <- unique(ldj_spe, by = "gr_sample_id")
        ldj_gen <- unique(ldj_gen, by = "gr_sample_id")
        
        spe_sxs =  splist2presabs(data = ld_spe, sites.col = 1, sp.col = 2) %>% setDT
        gen_sxs =  splist2presabs(data = ld_gen, sites.col = 1, sp.col = 2) %>% setDT
        
        ld2_spe <- spe_sxs[ldj_spe, on = "gr_sample_id"]
        ld2_gen <- spe_sxs[ldj_spe, on = "gr_sample_id"]
        
        ls_spe[[i]] = ld2_spe
        ls_gen[[i]] = ld2_gen
        rm(ld_spe,ld_gen,ldj_spe,i,ldj_gen,spe_sxs,gen_sxs,ld2_spe,ld2_gen);gc()

}

# Remove rare taxa --------------------------------------------------------

# rare taxa are taxa that occur in less that 1% of sampling sites. 


ls_rare = readRDS(file.path(DIR$pd, "008_a_rare_names.RDS"))

for (i in seq_along(ls_RT)) {
        dt_loop = ls_spe[[i]]
        ch_names_fixed = str_replace_all(
                string = names(dt_loop),
                pattern = "\\.",
                replacement = " "
        )
        
        in_loop_id = which(ch_names_fixed %in% ls_rare[[1]])
        
        dt_loop = dt_loop[, -in_loop_id, with = FALSE]
        ls_spe[[i]] = dt_loop
        rm(i, dt_loop, ch_names_fixed, in_loop_id)
}

rm(ls_rare);gc

# Are there any entires with only two columns i.e. no taxa ?
for (i in seq_along(ls_RT)) {
        dt_spe = ls_spe[[i]]
        dt_gen = ls_gen[[i]]
        
        if (ncol(dt_spe) < 3) print("species in", names(ls_RT)[i])
        if (ncol(dt_gen) < 3) print("genera in", names(ls_RT)[i])

        rm(dt_spe,dt_gen,i);gc()
}

# Save data to file ---------------------------------------------------

for (i in seq_along(ls_RT)) {
        
        dt_save_spe = ls_spe[[i]]
        dt_save_gen = ls_gen[[i]]
        ch_save_spe = paste0(names(ls_RT)[i],"_species.RDS")
        ch_save_gen = paste0(names(ls_RT)[i],"_genus.RDS")
        saveRDS(object = dt_save_spe,
                file = file.path(DIR$save, ch_save_spe))
        saveRDS(object = dt_save_gen,
                file = file.path(DIR$save, ch_save_gen))
} 

## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
