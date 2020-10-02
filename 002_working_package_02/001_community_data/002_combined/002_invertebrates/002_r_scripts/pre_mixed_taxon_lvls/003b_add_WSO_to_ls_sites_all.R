# ---------------------------------------------- #
### --- Add WSO1_ID to Lyche Solheim Sites --- ### 
# ---------------------------------------------- #

# date: 26.03.20 
# Jonathan Jupke 


# Setup -------------------------------------------------------------------

pacman::p_load(sf, dplyr)

w1 <- "~/01_Uni/03_GetReal/001_WP_01/001_Stream_Network/01_CCM2/03_data_processed/Catchment/"
w2 <- "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/"

# load data  --------------------------------------------------------------

setwd(w1)
cat <- st_read("2019-06-05_allGRcountires_WGS84.gpkg")

setwd(w2)
mzb <- readRDS("003_2020-03-31_mzb_sites_close_to_ls_large.RDS")


# spatial join  -----------------------------------------------------------

st_crs(cat)
mzb <- st_transform(mzb, crs = 4326)

cat <- dplyr::select(cat, WSO1_ID)

join1 <- st_join(x = mzb,
        y = cat,
        join = st_intersects)

saveRDS(join1, "003b_2020-03-31_mzb_sites_close_to_ls_all_withWSO.RDS")
