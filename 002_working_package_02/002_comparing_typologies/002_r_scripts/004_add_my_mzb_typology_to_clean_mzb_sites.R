# ---------------------------------------------- #
### --- Fit Diatom Data to My MZB Typology --- ### 
# ---------------------------------------------- #

#date: 03.03.20

# Setup -------------------------------------------------------------------

pacman::p_load(sf, data.table, magrittr, dplyr, purrr, geosphere)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")

# load data ---------------------------------------------------------------

mzb_sites <- readRDS("003_processed_data/004_2020-03-18_mzb_sites_low.RDS")

# my mzb 
ls <- st_read("../../../../../03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/003_typologies/2020-02-26_typology_gdm_inv_noxy_rivers.gpkg")

# clean data --------------------------------------------------------------

# needs to be in WGS84 for dist2Line()
ls %<>% st_transform(crs = 4326)

# remove far away sites ---------------------------------------------------

nn = st_nearest_feature(mzb_sites, ls); beepr::beep()

distance_list = list()

mzb_sites$distance_to_river = numeric(nrow(mzb_sites))

distance_to_river <- function(index) {
        as.numeric(
                dist2Line(p = st_coordinates(mzb_sites[index,]), 
                          line = st_coordinates(ls[nn[index],])[, 1:2])[1,1]
        )
}

mzb_sites$distance_to_river <- map_dbl(.x = 1:nrow(mzb_sites), distance_to_river); beepr::beep()

# add cluster id to sample id ---------------------------------------------

my_dia_typology = ls[nn, "cluster"] %>% 
        st_drop_geometry() %>% 
        as.list 

mzb_sites %<>% mutate(my_dia_typology = my_dia_typology$cluster)

mzb_sites2 <- mzb_sites %>% filter(distance_to_river < 500)
mzb_sites2 %<>% select(-distance_to_river)


# save to file  ------------------------------------------------------------
saveRDS(object = mzb_sites2, 
        file = paste0("../../../002_comparing_typologies/003_processed_data/001_",Sys.Date(),"_clean_mzb_with_my_mzb_typology.RDS"))

