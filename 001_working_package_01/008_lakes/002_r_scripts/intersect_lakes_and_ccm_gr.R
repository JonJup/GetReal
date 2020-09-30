# --------------------------------------------- #
### --- Add Lake column to catchment data --- ### 
# --------------------------------------------- #


# GR WP 1
# 29.05.19 + 08.08.19


# Setup -------------------------------------------------------------------

pacman::p_load(sf, dplyr, tmap, purrr)

setwd(here::here(""))

# Load Data ---------------------------------------------------------------

catchments = st_read("../01_stream_network/01_CCM2/03_data_processed/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")
rivers = st_read("../01_stream_network/01_CCM2/03_data_processed/Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
lakes = st_read("01_data_raw/EcrLak.sqlite")



# Visualizing the intersection --------------------------------------------
i = sample(catchments$WSO1_ID, 1)

cat1 = filter(catchments, WSO1_ID == i) %>% st_transform(crs = st_crs(lakes))
riv1 = filter(rivers, WSO1_ID == i) %>% st_transform(crs = st_crs(lakes))
(lake1 = st_intersection(x = lakes, y = cat1))
c <- st_intersection(riv1,lake1)
lake_cast = st_cast(lake1, to = "LINESTRING")

tm_shape(cat1) + tm_polygons() +
        tm_shape(riv1) + tm_lines() + 
        tm_shape(lakes) + tm_polygons()



# determine relative length - loop ----------------------------------------

## -- only first run -- ## 
cat2 = st_transform(catchments, crs = 3035)
cat2$proportionRiver = 0
## --                -- ## 

## -- later runs -- ##
cat2 = readRDS("01_Uni/03_GetReal/01_WP_01/01_Stream_Network/02_Lakes/cat2.RDS")
## --            -- ## 
riv2 = st_transform(rivers, crs = 3035)
safe_int = safely(st_intersection)
#14537

for (i in 74513:nrow(cat2)) {
        
        lake_x_l = safe_int(x = lakes, y = cat2[i,])
        if (!(is.null(lake_x_l$error))) {
                lake_x_l = safe_int(x = st_buffer(lakes, dist = 0),
                                    y = st_buffer(cat2[i,], dist = 0))
        }
        lake_x = lake_x_l$result
        if (nrow(lake_x) == 0) {
        print(paste(i, "nope"))
                      next()
        }
        wso_cat = cat2[i, "WSO1_ID"] %>% pull(WSO1_ID)
        riv_sel = filter(riv2, WSO1_ID == wso_cat)
        river_x_lake = st_intersection(riv_sel, lake_x)
        
        if (nrow(river_x_lake) == 0) {
                print(paste(i, "nope"))
                next()
        } 
        cat2$proportionRiver[i] = st_length(river_x_lake)/st_length(riv_sel)
        print(i)
}


saveRDS(cat2, "lakes.RDS")

