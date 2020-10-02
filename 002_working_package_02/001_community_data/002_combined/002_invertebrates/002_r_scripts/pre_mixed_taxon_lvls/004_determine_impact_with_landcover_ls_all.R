#### ------------------------------------------------- ####
### --- Determine Impact via landcover - ls edition --- ### 
#### ------------------------------------------------- ####

# date: 26.02.20 

# setup -------------------------------------------------------------------

pacman::p_load(sf, dplyr, data.table, magrittr)
setwd("~/01_Uni/03_GetReal/001_WP_01/003_corine_land_cover/03_data_processed/")

# Load data ---------------------------------------------------------------

landcover <- readRDS("reduced_clc.RDS")

setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/")
bio <- readRDS("003b_2020-03-31_mzb_sites_close_to_ls_all_withWSO.RDS")
all_bio <- readRDS("003_2020-03-31_mzb_data_close_to_ls_large.RDS")
# Intersect or spatial join  ----------------------------------------------

bio_x_lc <- left_join(bio, landcover, by = "WSO1_ID")
bio_x_lc <- mutate(bio_x_lc, 
       impact_lc = ifelse(Sum2 + Sum1 >= 20, 0, 1))

table(bio_x_lc$impact_lc)

## -- join new lc impact column to all_bio 
bio_x_lc2 <- dplyr::select(bio_x_lc, 
                           gr_sample_id, 
                           impact_lc, 
                           WSO1_ID) %>% 
        st_drop_geometry %>% 
        setDT
all_bio2 <- left_join(x = all_bio,
                      y = bio_x_lc2,
                      by = "gr_sample_id")

all_bio2 %<>% 
        mutate(low_impact = ifelse(is.na(pristine), 
                                   impact_lc, 
                                   pristine))

all_bio2 %<>% 
        filter(gr_sample_id %in% bio$gr_sample_id)

all_bio2_low <- filter(all_bio2, 
                       low_impact == 1)

sites_low <- all_bio2_low
setDT(sites_low)
sites_low <- unique(sites_low, 
                    by = "gr_sample_id")

sites_low %<>% st_as_sf()
sites_low %<>% dplyr::select(gr_sample_id, WSO1_ID)

# Save to file  -----------------------------------------------------------
saveRDS(file = paste0("004c_",Sys.Date(), "_mzb_data_low_ls_all.RDS"),
        object = all_bio2)

saveRDS(file = paste0("004c_",Sys.Date(), "_mzb_sites_low_ls_all.RDS"),
        object = sites_low)

