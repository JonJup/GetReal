########################################
### --- Add WSO1_ID to GRDC data --- ###
########################################
# 10.07.19

pacman::p_load(sf, dplyr, data.table)
setwd(here::here())
discharge = st_read("03_Data_Processed/01_Discharge/GRDC_Stations.gpkg")
ccm = st_read("../01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")

int1 = st_join(x = discharge,
        y = ccm,
        join = st_intersects)

int2 = int1 %>% select(GRDC_Number, WSO1_ID)

saveRDS(int2, "GRDCtoWSO1ID.RDS")
