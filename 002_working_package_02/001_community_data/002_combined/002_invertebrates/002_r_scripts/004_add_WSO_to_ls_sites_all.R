# ---------------------------------------------- #
### --- Add WSO1_ID to Lyche Solheim Sites --- ### 
# ---------------------------------------------- #

# date: 26.03.20 
# used : 03.07.20
# Jonathan Jupke 


# Setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, here)

dir_mzb = "002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data"
dir_cat = "001_working_package_01/001_stream_network/003_processed_data/Catchment/"
# load data  --------------------------------------------------------------
cat <- st_read(file.path(here(dir_cat), "2019-06-05_allGRcountires_WGS84.gpkg"))
mzb =  readRDS(file.path(here(dir_mzb), "003_2020-11-03_mzb_sites_close_to_ls.RDS"))           


# spatial join  -----------------------------------------------------------
mzb  <- st_as_sf(mzb)
mzb  <- st_transform(mzb, crs = 4326)
cat  <- dplyr::select(cat, WSO1_ID)
join <- st_join(x = mzb,
                y = cat,
                join = st_intersects)

# save to file ------------------------------------------------------------
saveRDS(object = join, 
        file   = file.path(here(dir_mzb), 
                           paste0("003b_",Sys.Date(),"_mzb_sites_1585_close_to_ls_withWSO.RDS")
                           )
        )
