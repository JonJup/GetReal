#### --------------------------------------- ####
### --- Assign ls rivers to invertebrates --- ### 
#### --------------------------------------- ####


# 01 Setup ----------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, sf, purrr)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")

# 02 load data  -----------------------------------------------------------
# it doesn't matter which cluster solution I use. They all omitted rivers with discharge > 10 
rivers <- st_read("../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")
mzb    <- readRDS("003_processed_data/002_2020-03-18_mzb_sites_common_orders.RDS")
mzb_all <- readRDS("003_processed_data/002_2020-03-18_mzb_data_common_orders.RDS")

# 03 clean data -----------------------------------------------------------
mzb %<>% st_as_sf()
mzb = dplyr::select(mzb, gr_sample_id)

mzb = st_transform(mzb, crs = 3035)
rivers = dplyr::select(rivers, m_btype20c)
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
                              "ls_bd_20" = rivers_resorted$m_btype20c)

distance_table2 <- distance_table[nn_distance <= 500]
hist(distance_table2$nn_distance)


# Filter data -------------------------------------------------------------

mzb2 <- mzb %>% filter(gr_sample_id %in% distance_table2$gr_sample_id)
mzb3 <- left_join(mzb2, 
                  distance_table2, 
                  by = "gr_sample_id")

mzb4 <- mzb3 %>% filter(!ls_bd_20 %in% c("RT1", 
                                         "RT2", 
                                         "RT4", 
                                         "RT8", 
                                         "RT10", 
                                         "RT17", 
                                         "RT18"))
mzb5 <- mzb4 %>% dplyr::select(-nn_distance)

mzb_all2 <- mzb_all[gr_sample_id %in% mzb5$gr_sample_id]
mzb_all3 <- left_join(mzb_all2, 
                distance_table2)
mzb_all4 <- dplyr::select(mzb_all3, -nn_distance)

# Save to File  -----------------------------------------------------------

saveRDS(object = mzb5,
        file = paste0("003_processed_data/",
                      "003_",
                      Sys.Date(),
                      "_mzb_sites_close_to_ls_small.RDS"))

saveRDS(object = mzb_all4,
        file = paste0("003_processed_data/003_",
                      Sys.Date(),
                      "_mzb_data_close_to_ls_small.RDS"))
