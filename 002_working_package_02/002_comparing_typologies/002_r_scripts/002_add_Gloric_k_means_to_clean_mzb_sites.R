# --------------------------------------------- #
### --- Fit Diatom Data to GLORIC K Means --- ### 
# --------------------------------------------- #

#date: 26.03.20

# Setup -------------------------------------------------------------------

pacman::p_load(sf, data.table, magrittr, dplyr, purrr, geosphere)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates//")

# load data ---------------------------------------------------------------

mzb_sites <- readRDS("003_processed_data/004_2020-03-18_mzb_sites_low.RDS")

#gloric typlogogie
gloric <- st_read("../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/03_GloRiC_v10/GloRiC_v10_shapefile/GloRiC_v10.shp")

# clean data --------------------------------------------------------------

# needs to be in WGS84 for dist2Line()
gloric %<>% st_transform(crs = 4326)

# remove far away sites ---------------------------------------------------

#
nn = st_nearest_feature(mzb_sites, gloric); beepr::beep()

distance_list = list()

mzb_sites$distance_to_river = numeric(nrow(mzb_sites))

distance_to_river <- function(index) {
        as.numeric(
                dist2Line(p = st_coordinates(mzb_sites[index,]), 
                          line = st_coordinates(gloric[nn[index],])[, 1:2])[1,1]
        )
}

mzb_sites$distance_to_river <- map_dbl(.x = 1:nrow(mzb_sites), distance_to_river); beepr::beep()

# add cluster id to sample id ---------------------------------------------

Kmeans_30 = gloric[nn, "Kmeans_30"] %>% 
        st_drop_geometry() %>% 
        as.list 


mzb_sites %<>% mutate(Kmeans_30 = Kmeans_30$Kmeans_30)

mzb_sites2 <- mzb_sites %>% filter(distance_to_river < 500)
mzb_sites2 %<>% select(-distance_to_river)


# save to file  ------------------------------------------------------------
saveRDS(object = mzb_sites2, 
        file = paste0("../../../002_comparing_typologies/003_processed_data/001_",Sys.Date(),"_clean_mzb_with_gloric_kmeans.RDS"))

