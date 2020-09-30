# ---------------------------------------------- #
### --- Add WSO1_ID to Lyche Solheim Sites --- ### 
# ---------------------------------------------- #

# date: 26.03.20 
# used : 03.07.20
# Jonathan Jupke 


# Setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, here)
setwd(here())

# load data  --------------------------------------------------------------
cat <- st_read("../../../../001_WP_01/001_Stream_Network/01_CCM2/03_data_processed/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")
mzb <- readRDS("003_processed_data/003_2020-07-03_mzb_sites_close_to_ls.RDS")

# spatial join  -----------------------------------------------------------
mzb  <- st_as_sf(mzb)
mzb  <- st_transform(mzb, crs = 4326)
cat  <- dplyr::select(cat, WSO1_ID)
join <- st_join(x = mzb,
                y = cat,
                join = st_intersects)

# save to file ------------------------------------------------------------
saveRDS(join, paste0("003_processed_data/003b_",Sys.Date(),"_mzb_sites_1585_close_to_ls_withWSO.RDS"))
