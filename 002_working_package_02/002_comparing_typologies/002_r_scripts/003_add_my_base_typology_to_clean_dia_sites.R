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
all_dia_data <- readRDS("003_processed_data/2020-02-17_all_dia_with_lc.RDS")

#lyche solheim typlogogie
ls <- st_read("../../../../../03_GetReal/001_WP_01/009_create_typology/003_data_processed/2020-03-02_typology_15_noxy_nobio_river.gpkg")

# clean data --------------------------------------------------------------

# all dia data to add coordinates to site names 

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
duplicated(diatom_sites2$gr_sample_id) %>% sum 

diatom_sites2 <- st_as_sf(diatom_sites2)
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

cluster = ls[nn, "cluster"] %>% 
        st_drop_geometry() %>% 
        as.list 

WSO1_id <- ls[nn, ] %>%
        select(WSO1_ID) %>% 
        st_drop_geometry()
        
        

diatom_sites2 %<>% mutate(my_base_typology = cluster$cluster,
                          WSO1_ID = WSO1_id[,1])

diatom_sites3 <- diatom_sites2 %>% filter(distance_to_river < 1000)
diatom_sites3 %<>% select(- distance_to_river)


# save to file  ------------------------------------------------------------
saveRDS(object = diatom_sites3, 
        file = paste0("../../../002_comparing_typologies/003_processed_data//",Sys.Date(),"_clean_dia_with_my_base_typology.RDS"))

