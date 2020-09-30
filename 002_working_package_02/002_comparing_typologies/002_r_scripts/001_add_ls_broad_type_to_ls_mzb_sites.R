# -------------------------------------------- #
### --- Fit mzb Data to Lyche Solheim --- ### 
# -------------------------------------------- #

#date: 03.03.20



# Setup -------------------------------------------------------------------

pacman::p_load(sf, data.table, magrittr, dplyr, purrr, geosphere)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")

# load data ---------------------------------------------------------------

mzb_sites <- readRDS("003_processed_data/004b_2020-03-26_mzb_sites_low_ls.RDS")

#lyche solheim typlogogie
ls <- st_read("../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")

# clean data --------------------------------------------------------------

# needs to be in WGS84 for dist2Line()
ls %<>% st_transform(crs = 4326)

# remove far away sites ---------------------------------------------------

# nearest neighbour 
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

m_btype20c = ls[nn, "m_btype20c"] %>% 
        st_drop_geometry() %>% 
        as.list 

m_btype12  = ls[nn, "m_btype12"] %>% 
        st_drop_geometry() %>% 
        as.list 

mzb_sites %<>% mutate(ls_btype12 = m_btype12$m_btype12,
                      ls_btype20 = m_btype20c$m_btype20)

mzb_sites2 <- mzb_sites %>% filter(distance_to_river < 500)
mzb_sites2 %<>% select(-distance_to_river)


# save to file  ------------------------------------------------------------
saveRDS(object = mzb_sites2, 
        file = paste0("../../../002_comparing_typologies/003_processed_data/","001_",Sys.Date(),"_clean_ls_mzb_with_ls_broad_type.RDS"))

