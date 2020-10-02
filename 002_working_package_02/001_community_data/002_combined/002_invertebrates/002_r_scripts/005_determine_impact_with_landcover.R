#### ------------------------------------ ####
### --- Determine Impact via landcover --- ### 
#### ------------------------------------ ####

# date: 19.06.20
# used: 03.07.20 
# Jonathan Jupke 

# setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr, here)
setwd(here())

# Load data ---------------------------------------------------------------
landcover      <- readRDS("../../../../001_WP_01/003_corine_land_cover/03_data_processed/reduced_clc.RDS")
sites          <- readRDS("003_processed_data/003b_2020-07-03_mzb_sites_1585_close_to_ls_withWSO.RDS")
observations   <- readRDS("003_processed_data/003_2020-07-03_mzb_data_close_to_ls.RDS")

# Intersect or spatial join  ----------------------------------------------
sites_x_lc <- left_join(sites, 
                        landcover, 
                        by = "WSO1_ID")

sites_x_lc %<>% mutate(impact_lc = ifelse(Sum2 + Sum1 >= 20, 0, 1))
# table(sites_x_lc$impact_lc)

## -- join new lc impact column to all_bio 
sites_x_lc2 <- dplyr::select(sites_x_lc,
                             gr_sample_id,
                             impact_lc,
                             WSO1_ID) %>%
        st_drop_geometry %>%
        setDT

observations2 <- left_join(x = observations,
                           y = sites_x_lc2,
                           by = "gr_sample_id")

observations2 %<>% 
        mutate(low_impact = ifelse(is.na(pristine), 
                                   impact_lc, 
                                   pristine))

observations2 %<>% 
        filter(gr_sample_id %in% sites$gr_sample_id)

observations2_low <- filter(observations2, 
                       low_impact == 1)

sites_low <- observations2
setDT(sites_low)
sites_low <- unique(sites_low, 
                    by = "gr_sample_id")

sites_low %<>% st_as_sf()
sites_low %<>% dplyr::select(gr_sample_id, WSO1_ID, ls_bd_20)

# Save to file  -----------------------------------------------------------
setwd(here("003_processed_data"))
saveRDS(file = paste0("004_",Sys.Date(), "_mzb_data1585_low_impact.RDS"),
        object = observations2_low)

saveRDS(file = paste0("004_",Sys.Date(), "_mzb_sites1585_low_impact.RDS"),
        object = sites_low)

