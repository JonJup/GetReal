#### ------------------------------------ ####
### --- Determine Impact via landcover --- ### 
### --- FOR HMSC COURSE                --- ### 
#### ------------------------------------ ####

# date: 08.07.20
# used: 08.07.20 
# Jonathan Jupke 

# setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr, here)
setwd(here())

# Load data ---------------------------------------------------------------
landcover      <- readRDS("../../../../001_WP_01/003_corine_land_cover/03_data_processed/reduced_clc.RDS")
sites          <- readRDS("003_processed_data/003b_2020-07-03_mzb_sites_1585_close_to_ls_withWSO.RDS")
observations   <- readRDS("003_processed_data/003_2020-07-03_mzb_data_close_to_ls.RDS")

# Intersect or spatial join  ----------------------------------------------

# join landcover data sites sites via WSO1_ID variable 
sites_x_lc <- left_join(sites, 
                        landcover, 
                        by = "WSO1_ID")

# derive binary impact variable. If more than 20% catchment area are
# agricultural or urban assign 0 i.e. high impact. 
sites_x_lc %<>% mutate(impact_lc = ifelse(Sum2 + Sum1 >= 20, 0, 1))

# subset sites_x_lc to relevant variables, drop geometry column and transform into data.table 
sites_x_lc2 <- dplyr::select(sites_x_lc,
                             gr_sample_id,
                             impact_lc,
                             WSO1_ID) %>%
        st_drop_geometry %>%
        setDT

# now add the impact_lc variable to the observations via a left join of observations and sites_x_lc2
observations2 <- left_join(x = observations,
                           y = sites_x_lc2,
                           by = "gr_sample_id")

# new variable low_impact: wherever the pristine variable is not NA this is used
# to judge the impact at a site. Where pristine is NA impact_lc is used.
# low_impact is again a binary variable with 0 meaning high impact and 1 low. 
observations2[, low_impact := ifelse(is.na(pristine), impact_lc, pristine)]
# Subset to ? dont understand this anymore ... comment out does not change anything
#observations2[gr_sample_id %in% sites$gr_sample_id]
# Drop variables that were used to derive low_impact 
observations2[, c("pristine", "impact_lc") := NULL]

# get the new sites variables 
sites_new <- copy(observations2)
sites_new %<>% unique(by = "gr_sample_id")

# turn spatial
sites_new %<>% st_as_sf()
sites_new %<>% dplyr::select(gr_sample_id, WSO1_ID, ls_bd_20)

# Save to file  -----------------------------------------------------------
setwd(here("003_processed_data/HMSC/"))
saveRDS(file = paste0("001_",Sys.Date(), "_mzb_data1585_all_impact.RDS"),
        object = observations2)

saveRDS(file = paste0("001_",Sys.Date(), "_mzb_sites1585_all_impact.RDS"),
        object = sites_new)

