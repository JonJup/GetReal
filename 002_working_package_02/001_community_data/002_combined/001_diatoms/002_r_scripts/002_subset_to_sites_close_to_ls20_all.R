#### -------------------------------- ####
### --- Assign ls rivers to diatom --- ### 
#### -------------------------------- ####

# date: 03.04.20
# Jonathan Jupke 

# Every diatom site is allocated to one line segment of Lyche Solheim's European
# Stream Typology. Lyche Solheim et al. (2019): A new broad typology for rivers
# and lakes in Europe: Development and application for large-scale environmental
# assessments. Science of the total Environment 697 134043. All sites that are
# more than 500m away from there closest site are removed from the data set.

# 01 Setup ----------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, sf, purrr)
setwd(here(...="002_working_package_02/001_community_data/002_combined/001_diatoms/"))

# 02 load data  -----------------------------------------------------------
rivers    <- st_read("../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")
dia_sites <- st_read("003_processed_data/001_2020-06-25_all_dia_sites.gpkg")
dia_sites <- st_read("003_processed_data/001_2020-06-29_all_dia_sites_w_org_names.gpkg")
dia_all   <- readRDS("003_processed_data/001_2020-06-29_all_dia_data_w_org_names.RDS")

# 03 clean data -----------------------------------------------------------

dia_sites %<>% dplyr::select(gr_sample_id)

# EPSG 3035 is ETRS89 and geographic crs for Eurasia and recommended by the EU.
# As it is geographic I will later compute distances along the ellipsis. All
# points where this would make a difference are so far away from the next stream
# that it doesnt matter if its say 15 or 15.3 Kilometers. 
# see https://gis.stackexchange.com/questions/178201/calculate-the-distance-between-two-coordinates-wgs84-in-etrs89

dia_sites %<>% st_transform(crs = 3035)
rivers    %<>% dplyr::select(m_btype20c) %>% st_transform(crs = 3035)

# add catchment info to bio data ------------------------------------------

nn = st_nearest_feature(dia_sites , rivers); beepr::beep()
rivers_resorted <- rivers[nn,]

# Quality check 
rivers_resorted[1, ] == rivers[nn[1], ]
rivers_resorted[125, ] == rivers[nn[125], ]
rivers_resorted[3000, ] == rivers[nn[3000], ]


distance_list <-
        map(.x = 1:nrow(dia_sites),
            .f = ~ as.numeric(st_distance(x = dia_sites[.x, ],
                                          y = rivers_resorted[.x, ])));beepr::beep()

distance_list2 <- unlist(distance_list)
distance_table <- data.table("gr_sample_id" = dia$gr_sample_id,
                              "nn_distance" = distance_list2,
                              "ls_bd_20" = rivers_resorted$m_btype20c)

distance_table2 <- distance_table[nn_distance <= 500]
hist(distance_table2$nn_distance)


# Filter data -------------------------------------------------------------

dia2 <- dia_sites %>% filter(gr_sample_id %in% distance_table2$gr_sample_id)
dia3 <- left_join(dia2, 
                  distance_table2, 
                  by = "gr_sample_id")


dia4 <- dia3 %>% dplyr::select(-nn_distance)

dia_all2 <- dia_all %>% filter(gr_sample_id %in% dia4$gr_sample_id)
dia_all3 <- left_join(dia_all2, 
                distance_table2)
dia_all4 <- dplyr::select(dia_all3, -nn_distance)

# Save to File  -----------------------------------------------------------

saveRDS(object = dia4,
        file = paste0("003_processed_data/",
                      "002_",
                      Sys.Date(),
                      "_dia_sites_close_to_ls_large.RDS"))

saveRDS(object = dia_all4,
        file = paste0("003_processed_data/002_",
                      Sys.Date(),
                      "_dia_data_close_to_ls_large.RDS"))
