### --------------------- ###
# ---- 04 Map clusters ---- #
### --------------------- ###


# date 

# whats going on 

### --- OVERVIEW --- ### 
# 01. Setup
### ---------------- ### 


# 01. Setup ---------------------------------------------------------------


pacman::p_load(sf, 
               dplyr,
               data.table)
# also required: here 

setwd(here::here())
# or
setwd("~/01_Uni/03_GetReal/001_WP_01/001_Stream_Network/01_CCM2/03_data_processed/")
## data input 
ccm = st_read("Catchment/2019-06-13_allGRcountires_WGS84_withOV.gpkg")
riv = st_read("Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
ccm2 = dplyr::select(ccm, WSO1_ID)
riv2 = dplyr::select(riv, WSO1_ID)
beepr::beep()
# 02. data input  ---------------------------------------------------------

# input_cluster = readRDS("03_data_processed/tc_sc4_14_ID.RDS")
# input_cluster = readRDS("03_data_processed/200107_tc_sc4_noxy_25_ID.RDS")
# input_cluster = readRDS("03_data_processed/2020-01-14_tc_sc4_xy_19.RDS")
# input_cluster = readRDS("03_data_processed/2020-01-30_tc_sc4_f2f_12.RDS")
# input_cluster = readRDS("03_data_processed/2020-02-05_tc_sc4_12_after_weight.RDS")
# input_cluster = readRDS("03_data_processed/2020-02-06_tc_sc4_12_w2.RDS")
# input_cluster = readRDS("03_data_processed/2020-02-06_tc_sc4_12_w3.RDS")
# input_cluster = readRDS("03_data_processed/2020-02-14_typology_15.RDS")
# input_cluster = readRDS("03_data_processed/2020-02-14_typology_30.RDS")
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/")
input_cluster = readRDS("007_2020-03-25_typology11_gdm_inv_noxy.RDS")

# 03.Join cluster id to spatial -------------------------------------------

join_cat = left_join(ccm2, input_cluster, by = "WSO1_ID")
join_riv = left_join(riv2, input_cluster, by = "WSO1_ID")

# 04. Not Clustered? Cluster = 0 ------------------------------------------

join_cat$cluster[which(is.na(join_cat$cluster))] = 0
join_riv$cluster[which(is.na(join_riv$cluster))] = 0

# 05. Save to File  -------------------------------------------------------

saveRDS(join_riv, paste0 ("008_", Sys.Date(),"_typology_11_noxy_inv_river.RDS"))
saveRDS(join_cat, paste0 ("008_", Sys.Date(),"_typology_11_noxy_inv_catchment.RDS"))
st_write(join_riv, paste0("008_", Sys.Date(),"_typology_11_noxy_inv_river.gpkg"))
beepr::beep()