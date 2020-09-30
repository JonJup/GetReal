# ----------------------------------------------- #
### --- Fit Diatom Data to My Base Typology --- ### 
# ----------------------------------------------- #

#date: 26.03.20

# Setup -------------------------------------------------------------------

pacman::p_load(sf, data.table, magrittr, dplyr, purrr, geosphere)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates//")

# load data ---------------------------------------------------------------

mzb_sites <- readRDS("003_processed_data/004_2020-03-18_mzb_sites_low.RDS")

# my base typology 
ls <- st_read("../../../../../03_GetReal/001_WP_01/009_create_typology/003_data_processed/2020-03-02_typology_15_noxy_nobio_river.gpkg")

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

cluster = ls[nn, "cluster"] %>% 
        st_drop_geometry() %>% 
        as.list 

WSO1_id <- ls[nn, ] %>%
        select(WSO1_ID) %>% 
        st_drop_geometry()
        
        

mzb_sites %<>% mutate(my_base_typology = cluster$cluster,
                          WSO1_ID = WSO1_id[,1])

mzb_sites2 <- mzb_sites %>% filter(distance_to_river < 500)
mzb_sites2 %<>% select(-distance_to_river)


# save to file  ------------------------------------------------------------
saveRDS(object = mzb_sites2, 
        file = paste0("../../../002_comparing_typologies/003_processed_data/001_",Sys.Date(),"_clean_mzb_with_my_base_typology.RDS"))

