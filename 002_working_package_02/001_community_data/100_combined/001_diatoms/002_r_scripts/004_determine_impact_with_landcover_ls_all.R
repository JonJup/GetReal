#### ------------------------------------------------- ####
### --- Determine Impact via landcover - ls edition --- ### 
### -------------------- Diatoms ---------------------- ###
#### ------------------------------------------------- ####

# date written: 03.04.20 
# dates used: ..., 30.06.20
# Jonathan Jupke 

# Some data sets contained information on environmental conditions that allowed
# us to differentiate between less impacted and more impacted sites. Other data
# set had no such information. For these we use a heuristic: If the area in the
# catchtent that is taken up by agriculture or urban spaces is below 20% it has
# low impact.

# setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr, here)
setwd(here())

# Load data ---------------------------------------------------------------
landcover <- readRDS("../../../../001_WP_01/003_corine_land_cover/03_data_processed/reduced_clc.RDS")
sites <- readRDS("003_processed_data/003_2020-06-30_dia_sites_close_to_ls_all_withWSO.RDS")
diato <- readRDS("003_processed_data/002_2020-06-30_dia_data_close_to_ls_large.RDS")

# quality check  ----------------------------------------------------------
sum(diato$gr_sample_id %in% sites$gr_sample_id) == nrow(diato)
sum(sites$gr_sample_id %in% diato$gr_sample_id) == nrow(sites)
# -> passed 

# join land cover to sites  ----------------------------------------------
sites_x_lc <- left_join(sites, landcover, by = "WSO1_ID")

# qs -> passed 
nrow(sites_x_lc) == nrow(sites)

sites_x_lc <- mutate(sites_x_lc, impact_lc = ifelse(Sum2 + Sum1 >= 20, 0, 1))

# join sites with diatom data agian ---------------------------------------
sites_x_lc_2 <- dplyr::select(sites_x_lc, 
                           gr_sample_id, 
                           impact_lc, 
                           WSO1_ID) %>% 
        st_drop_geometry %>% 
        setDT

# qs -> passed 
sum(diato$gr_sample_id %in% sites_x_lc_2$gr_sample_id) == nrow(diato)
sum(sites_x_lc$gr_sample_id %in% diato$gr_sample_id) == nrow(sites_x_lc)
sum(sites_x_lc_2$gr_sample_id %in% diato$gr_sample_id) == nrow(sites_x_lc_2)

diato_2 <- left_join(x = diato,
                     y = sites_x_lc_2,
                     by = "gr_sample_id")

# qs -> passed 
sum(diato_2$gr_sample_id %in% sites_x_lc_2$gr_sample_id) == nrow(diato_2) &
sum(sites_x_lc$gr_sample_id %in% diato_2$gr_sample_id) == nrow(sites_x_lc) &
sum(sites_x_lc_2$gr_sample_id %in% diato_2$gr_sample_id) == nrow(sites_x_lc_2)

# add low impact column 
diato_2 %<>% 
        mutate(low_impact = ifelse(is.na(pristine), 
                                   impact_lc, 
                                   pristine))

# subset to low impact sites 
diato_2_low <- filter(diato_2, 
                       low_impact == 1)

# derive low impact sites from low impact diatom data
sites_low <- diato_2_low
setDT(sites_low)
sites_low <- unique(sites_low, 
                    by = "gr_sample_id")

# qs -> passed 
sum(diato_2_low$gr_sample_id  %in% sites_low$gr_sample_id) == nrow(diato_2_low) & 
sum(sites_low$gr_sample_id   %in% diato_2_low$gr_sample_id)  == nrow(sites_low) &
sum(sites_low$gr_sample_id %in% diato_2_low$gr_sample_id)  == nrow(sites_low)

# transfrom sites to spacial object and subset to relevant columns 
sites_low %<>% st_as_sf()
sites_low %<>% dplyr::select(gr_sample_id, WSO1_ID)

# Save to file  -----------------------------------------------------------
saveRDS(file = paste0("003_processed_data/004_",Sys.Date(), "_dia_data_low_ls_all.RDS"),
        object = diato_2_low)

saveRDS(file = paste0("003_processed_data/004_",Sys.Date(), "_dia_sites_low_ls_all.RDS"),
        object = sites_low)

