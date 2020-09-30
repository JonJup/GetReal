#####################################
### --- Where do stations lie --- ### 
#####################################

# 09.07.19

# I filter the discharge dataset to all observations in the time span
# 2000-2013 and then aggregate to only one observation per station.

# Setup -------------------------------------------------------------------

pacman::p_load(data.table,
               sf, magrittr)

dis = fread("01_Uni/03_GetReal/01_WP_01/04_Modelling Discharge/03_Data_Processed/01_Discharge/discharge_work.csv")
dis = dis[Year <= 2013 & Year >= 2000]


unique(dis$Longitude) %>% length
unique(dis$Latitude) %>% length
dis[,.(Longitude, Latitude), by = GRDC_Number]
dis2 = unique(dis, by = "GRDC_Number")

dis3 = st_as_sf(dis2, coords = c("Longitude", "Latitude")) %>% st_set_crs(4326)
st_write(dis3, "GRDC_Stations.gpkg")


sort(unique(dis$River_Name))
