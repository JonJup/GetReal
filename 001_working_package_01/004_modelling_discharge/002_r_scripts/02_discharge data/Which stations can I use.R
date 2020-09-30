#########################################
### --- Which stations can I use? --- ###
#########################################

# 11.07.19

# Some of the discharge measurements are probably taken at small streams that
# are not represented in CCM. To remove these observations from the dataset I
# will reduce it to only observations which are maximally 200m away from a
# stream in CCM2


# 01. Setup ---------------------------------------------------------------

pacman::p_load(sf, dplyr, data.table)
setwd(here::here())

# load data 
ccm_streams = st_read("../01_Stream_Network/01_CCM2/02_GPKG/Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
stations = readRDS("03_Data_Processed/01_Discharge/19-07-11_discharge_combo.RDS") %>% setDT()
# connects IDs from Stations to ID of catchments 
IDtoID = readRDS("03_Data_Processed/01_Discharge/combined/Stations_ID_to_WSO1ID.RDS") %>% 
      st_drop_geometry()

# Clean -------------------------------------------------------------------

# filter to years 2000 - 2013
stations = stations[Year >= 2000 & Year <= 2013]
# one observation per station
stations.u = unique(stations, by = "ID")
# produce spatial object for interection 
stations.st = st_as_sf(stations.u, coords = c("Longitude", "Latitude"), crs = 4326) 
# transform to crs of ccm 
stations_t = st_transform(stations.st, crs = st_crs(ccm_streams))

# check that IDs are unique -- yup 
length(unique(stations_t$ID)) == nrow(stations_t)

ccm_200m = st_buffer(ccm_streams, dist = 200)

## --  intersections If I just intersect the buffered rivers with the stations I
#get duplciates. To prevent this from happening I use the Stations_ID_to_WSO1ID
#file which holds catchment IDs and station IDs. The intersected dataset will be
#filtered to only hold row where WSO1ID of the stream and that of the point
#match .

# join WSO1_ID to stations 
stations_t2 = left_join(
      x = stations_t,
      y = IDtoID)

# intersect and filter 
int200c = st_intersection(stations_t2, ccm_200m) %>% 
      filter(WSO1_ID == WSO1_ID.1)

# check that IDs are unique, i.e. no duplicates 
length(unique(int200c$ID)) - nrow(int200c)

# extract IDs of usable stations -- 1724 from 2130 
ID.to.use = int200c$ID

# fiter stations 
stations2 = stations[ID %in% ID.to.use]

# save to file 
saveRDS(stations2, "discharge_combined_and_filtered.RDS")
