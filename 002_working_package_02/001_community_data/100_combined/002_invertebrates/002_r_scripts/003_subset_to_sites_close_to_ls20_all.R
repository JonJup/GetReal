#### --------------------------------------- ####
### --- Assign ls rivers to invertebrates --- ### 
#### --------------------------------------- ####


# 01 Setup ----------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, sf, purrr, here, beepr)
setwd(here())

# 02 load data  -----------------------------------------------------------
 
rivers         <- st_read("../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")
sites          <- readRDS("003_processed_data/002_2020-06-23_all_inv_sites15_85.RDS") 
observations   <- readRDS("003_processed_data/002_2020-06-23_all_inv_15_85.RDS") 

# 03 clean data -----------------------------------------------------------
sites   %<>% st_as_sf()
sites  %<>% dplyr::select(gr_sample_id)
sites  %<>% st_transform(crs = 3035)
rivers %<>% dplyr::select(m_btype20c)
rivers %<>% st_transform(crs = 3035)
setDT(observations)
# add catchment info to bio data ------------------------------------------

nn = st_nearest_feature(sites, rivers); beep()
rivers_resorted <- rivers[nn,]

# Quality check 
rivers_resorted[1, ]    == rivers[nn[1], ]
rivers_resorted[125, ]  == rivers[nn[125], ]
rivers_resorted[3000, ] == rivers[nn[3000], ]


distance_list <-
        map(.x = 1:nrow(sites),
            .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                          y = rivers_resorted[.x, ]))); beep()

distance_list2 <- unlist(distance_list)
distance_table <- data.table("gr_sample_id" = sites$gr_sample_id,
                              "nn_distance" = distance_list2,
                              "ls_bd_20"    = rivers_resorted$m_btype20c)

distance_table2 <- distance_table[nn_distance <= 500]
hist(distance_table2$nn_distance)


# Filter data -------------------------------------------------------------

sites2 <- sites %>% filter(gr_sample_id %in% distance_table2$gr_sample_id)
sites3 <- left_join(sites2, 
                  distance_table2, 
                  by = "gr_sample_id")


sites4 <- sites3 %>% dplyr::select(-nn_distance)

observations2 <- observations[gr_sample_id %in% sites4$gr_sample_id]
observations3 <- left_join(observations2, distance_table2)
observations3 %<>% dplyr::select(-nn_distance)

# Save to File  -----------------------------------------------------------

saveRDS(object = sites4,
        file = paste0("003_processed_data/",
                      "003_",
                      Sys.Date(),
                      "_mzb_sites_close_to_ls.RDS"))

saveRDS(object = observations3,
        file = paste0("003_processed_data/003_",
                      Sys.Date(),
                      "_mzb_data_close_to_ls.RDS"))
