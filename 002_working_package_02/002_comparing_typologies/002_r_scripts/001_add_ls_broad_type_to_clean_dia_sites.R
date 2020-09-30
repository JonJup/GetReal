# -------------------------------------------- #
### --- Fit Diatom Data to Lyche Solheim --- ### 
# -------------------------------------------- #

#date: 03.03.20



# Setup -------------------------------------------------------------------

pacman::p_load(sf, data.table, magrittr, dplyr, purrr, geosphere)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/001_Diatoms/")

# load data ---------------------------------------------------------------

#diatom sampling sites - only character vector 
diatom_sites <- readRDS("003_processed_data/001_speciesXsites_tables/sites.bio2.s.all.RDS")
diatom_sites <- data.table("gr_sample_id" = diatom_sites )


#lyche solheim typlogogie
ls <- st_read("../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp/m_river_fec_broad_type.shp")

# clean data --------------------------------------------------------------

# all dia data to add coordinates to site names 
all_dia_data <- readRDS("003_processed_data/2020-02-17_all_dia_with_lc.RDS")
all_dia_data$gr_sample_id <- as.character(all_dia_data$gr_sample_id)
# collapse to one row per site 
all_dia_data2 <- all_dia_data
setDT(all_dia_data2)
all_dia_data2 %<>% unique(by = "gr_sample_id")
sum(all_dia_data2$gr_sample_id %in% diatom_sites$gr_sample_id)

diatom_sites2 <- all_dia_data2[diatom_sites,
                               on = "gr_sample_id"]

# all dia data to add coordinates to site names 
all_dia_data <- readRDS("003_processed_data/2020-02-17_all_dia_with_lc.RDS")
all_dia_data$gr_sample_id <- as.character(all_dia_data$gr_sample_id)
# collapse to one row per site 
all_dia_data2 <- all_dia_data
setDT(all_dia_data2)
all_dia_data2 %<>% unique(by = "gr_sample_id")
sum(all_dia_data2$gr_sample_id %in% diatom_sites$gr_sample_id)

diatom_sites2 <- all_dia_data2[diatom_sites,
                               on = "gr_sample_id"]

# all dia data to add coordinates to site names 
all_dia_data <- readRDS("003_processed_data/2020-02-17_all_dia_with_lc.RDS")
all_dia_data$gr_sample_id <- as.character(all_dia_data$gr_sample_id)
# collapse to one row per site 
all_dia_data2 <- all_dia_data
setDT(all_dia_data2)
all_dia_data2 %<>% unique(by = "gr_sample_id")
sum(all_dia_data2$gr_sample_id %in% diatom_sites$gr_sample_id)

diatom_sites2 <- all_dia_data2[diatom_sites,
                               on = "gr_sample_id"]

diatom_sites2 <- diatom_sites2[,.(gr_sample_id, geom)]

# no duplicates 
duplicated(diatom_sites2$gr_sample_id) %>% sum <- diatom_sites2[,.(gr_sample_id, geom)]

diatom_sites2 <- st_as_sf(diatom_sites2)
diatom_sites2 <- st_transform(diatom_sites2, crs = st_crs(ls))
ls %<>% st_transform(crs = 4326)
# remove far away sites ---------------------------------------------------

nn = st_nearest_feature(diatom_sites2, ls); beepr::beep()

distance_list = list()

diatom_sites2$distance_to_river = numeric(nrow(diatom_sites2))

distance_to_river <- function(index) {
        as.numeric(
                dist2Line(p = st_coordinates(diatom_sites2[index,]), 
                          line = st_coordinates(ls[nn[index],])[, 1:2])[1,1]
        )
}

diatom_sites2$distance_to_river <- map_dbl(.x = 1:nrow(diatom_sites2), distance_to_river); beepr::beep()

# add cluster id to sample id ---------------------------------------------

m_btype20c = ls[nn, "m_btype20c"] %>% 
        st_drop_geometry() %>% 
        as.list 

m_btype12  = ls[nn, "m_btype12"] %>% 
        st_drop_geometry() %>% 
        as.list 

diatom_sites2 %<>% mutate(ls_btype12 = m_btype12$m_btype12,
                          ls_btype20 = m_btype20c$m_btype20c)

diatom_sites3 <- diatom_sites2 %>% filter(distance_to_river < 1000)
diatom_sites3 %<>% select(- distance_to_river)


# save to file  ------------------------------------------------------------
saveRDS(object = diatom_sites3, 
        file = paste0("../../../002_comparing_typologies/003_processed_data//",Sys.Date(),"_clean_dia_with_ls_broad_type.RDS"))

