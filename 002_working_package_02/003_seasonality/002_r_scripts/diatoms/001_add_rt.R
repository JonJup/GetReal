### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Diatoms --------------------------- --- ###
### --- Add stream type to diatom data -------- ###
### ------------------------------------------- ###

# date created: 01.09.20
# date used:    01.09.20 

# Add the Lyche Solheim Stream type to combined and cleaned diatom sample data. 

# setup -------------------------------------------------------------------

pacman::p_load(sf, here, magrittr, purrr)
setwd(here())

# load data ----------------------------------------------------------------

data <- readRDS("../001_Community Data/100_Combined/001_Diatoms/003_processed_data/007_2020-08-18all_dia_optimal_taxon_75.RDS")
stre <- st_read("../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")

# prepare analysis --------------------------------------------------------

sites <- copy(data)
sites <- unique(sites, by = "gr_sample_id")
sites %<>% st_as_sf()

if (!st_crs(stre) == st_crs(sites))
        sites <- st_transform(x = sites, crs = st_crs(stre))

nn            <- st_nearest_feature(sites , stre); beepr::beep()
strr          <- stre[nn,]
distance_list <-
        map(.x = 1:nrow(sites),
            .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                          y = strr[.x, ])));beepr::beep()
distance_list2 <- unlist(distance_list)
distance_table <- data.table("gr_sample_id" = sites$gr_sample_id,
                             "nn_distance"  = distance_list2,
                             "rt"           = strr$m_btype20c)

distance_table2 <- distance_table[nn_distance <= 100]

setDT(sites)

sites2 <- left_join(distance_table2, 
                    sites)

sites2 <- sites2[, c("gr_sample_id", "rt")]

data2 <- sites2[data, on = "gr_sample_id"]

saveRDS(object = data2, 
        file   = paste0("003_results/diatoms/001_", Sys.Date(), "_diatoms_w_rt.RDS"))

## -- ## 
if (readline("remove all ") == "yes") rm(list = ls())        
