# ---------------------------------------------- #
### --- Add WSO1_ID to Lyche Solheim Sites --- ###
### ---------------- Diatoms ----------------- ##'
# ---------------------------------------------- #

# date written: 03.04.20 
# dates used  : ..., 30.06.20 
# Jonathan Jupke 

# Setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, here)
setwd(here())

# load data  --------------------------------------------------------------
cat <- st_read("../../../../001_WP_01/001_Stream_Network/01_CCM2/03_data_processed/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")
dia <- readRDS("003_processed_data/002_2020-06-30_dia_sites_close_to_ls_large.RDS")

# spatial join  -----------------------------------------------------------
dat <- st_transform(dia, crs = 4326)
cat <- dplyr::select(cat, WSO1_ID)
join1 <- st_join(x = dat,
        y = cat,
        join = st_intersects)

saveRDS(object = join1,
        file = paste0(
                "003_processed_data/003_",
                Sys.Date(),
                "_dia_sites_close_to_ls_all_withWSO.RDS"
                )
)
