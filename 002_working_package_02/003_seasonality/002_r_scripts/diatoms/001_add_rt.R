### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Diatoms --------------------------- --- ###
### --- Add stream type to diatom data -------- ###
### ------------------------------------------- ###

# date created: 01.09.20
# date used:    01.09.20, 09.11

# Add the Lyche Solheim Stream type to combined and cleaned diatom sample data. 

# setup -------------------------------------------------------------------

pacman::p_load(data.table, 
               here, 
               magrittr, 
               purrr, 
               sf)

dir_dia = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data//")
dir_riv = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al"
dir_save = here("002_working_package_02/003_seasonality/003_results/diatoms/")
save =F 
# load data ----------------------------------------------------------------

dt_dia = readRDS(file.path(dir_dia, "007_2020-11-04all_dia_optimal_taxon_75.RDS"))
st_riv = st_read(file.path(dir_riv, "m_river_fec_broad_type.shp"))

# prepare analysis --------------------------------------------------------

dt_sites = copy(dt_dia)
dt_sites = unique(dt_sites, by = "gr_sample_id")
st_sites = st_as_sf(dt_sites)
if (!st_crs(st_riv) == st_crs(st_sites))
        st_sites = st_transform(x = st_sites, crs = st_crs(st_riv))

nn      = st_nearest_feature(st_sites , st_riv); beepr::beep()
st_riv2 = st_riv[nn,]
distance_list =
        map(.x = 1:nrow(st_sites),
            .f = ~ as.numeric(st_distance(x = st_sites[.x, ],
                                          y = st_riv2[.x, ])));beepr::beep()
distance_list2 <- unlist(distance_list)
distance_table <- data.table("gr_sample_id" = st_sites$gr_sample_id,
                             "nn_distance"  = distance_list2,
                             "rt"           = st_riv2$m_btype20c)

distance_table2 = distance_table[nn_distance <= 100]

dt_sites = left_join(distance_table2, 
                     dt_sites)

dt_sites = dt_sites[, c("gr_sample_id", "rt")]

dt_dia2 <- dt_sites[dt_dia, on = "gr_sample_id"]

if (save) {
        saveRDS(object = dt_dia2,
                file   = file.path(
                        dir_save,
                        paste0("001_", Sys.Date(), "_diatoms_w_rt.RDS")
                ))
}

## -- ## 
if (readline("remove all ") == "yes") rm(list = ls())        
