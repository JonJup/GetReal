#### ---------------------------------------- ####
### --- Assign Catchments to invertebrates --- ### 
#### ---------------------------------------- ####


# 01 Setup ----------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, sf, purrr)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")

# 02 load data  -----------------------------------------------------------
# it doesn't matter which cluster solution I use. They all omitted rivers with discharge > 10 
rivers <- st_read("../../../../001_WP_01/009_create_typology/003_data_processed/2020-03-02_typology_15_noxy_nobio_river.gpkg")
mzb    <- readRDS("003_processed_data/002_2020-03-18_mzb_sites_common_orders.RDS")
mzb_all <- readRDS("003_processed_data/002_2020-03-18_mzb_data_common_orders.RDS")

# 03 clean data -----------------------------------------------------------
mzb %<>% st_as_sf()
mzb = select(mzb, gr_sample_id)

mzb = st_transform(mzb, crs = 3035)
rivers = select(rivers, cluster, WSO1_ID)
rivers = st_transform(rivers, crs = 3035)

# add catchment info to bio data ------------------------------------------

nn = st_nearest_feature(mzb, rivers); beepr::beep()

rivers_resorted <- rivers[nn,]

# Quality check 
rivers_resorted[1, ] == rivers[nn[1], ]
rivers_resorted[125, ] == rivers[nn[125], ]
rivers_resorted[3000, ] == rivers[nn[3000], ]


distance_list <-
        map(.x = 1:nrow(mzb),
            .f = ~ as.numeric(st_distance(x =mzb[.x, ],
                                          y = rivers_resorted[.x, ])));beepr::beep()

distance_list2 <- unlist(distance_list)
distance_table <- data.table("gr_sample_id" = mzb$gr_sample_id,
                              "nn_distance" = distance_list2,
                              "WSO1_ID" = rivers_resorted$WSO1_ID,
                              "cluster" = rivers_resorted$cluster)

distance_table2 <- distance_table[nn_distance <= 500]
hist(distance_table2$nn_distance)


# Filter data -------------------------------------------------------------

mzb2 <- mzb %>% filter(gr_sample_id %in% distance_table2$gr_sample_id)
mzb3 <- left_join(mzb, distance_table2)
mzb4 <- mzb3 %>% filter(cluster != 0)
mzb5 <- select(mzb4, gr_sample_id, WSO1_ID)

mzb_all2 <- mzb_all[gr_sample_id %in% mzb5$gr_sample_id]
mzb_all3 <- left_join(mzb_all2, 
                distance_table2)
mzb_all4 <- select(mzb_all3, -nn_distance)

# Save to File  -----------------------------------------------------------

saveRDS(object = mzb5,
        file = paste0("003_processed_data/",
                      "003_",
                      Sys.Date(),
                      "_mzb_sites_close_to_river.RDS"))

saveRDS(object = mzb_all4,
        file = paste0("003_processed_data/003_",
                      Sys.Date(),
                      "_mzb_data_close_to_river.RDS"))
