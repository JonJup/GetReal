#################################
### --- Clean cat clc all --- ### 
#################################

# date : 11.09.19 

#This script takes the cat_clc_all.RDS file which holds percentage values of
#land cover classes per catchment and summarizes the latter in borader
#categories. CLC follows a code such like 3 - Forest and semi natural areas; 32
#- Scrub and/or herbaceous vegetation associations; 323 - Sclerophyllous
#vegetation. This resolution is greater that what we need. Hence I summarize
#them at the first level.

# 01. Setup ---------------------------------------------------------------

pacman::p_load(data.table, dplyr, sf)
data = readRDS(here::here("03_data_processed/cat_clc_all.RDS"))
data = setDT(data)


# 02. Sum data to first number --------------------------------------------

for (i in 1:5) {
      
      colnames = paste0("Sum",1:5)
      data[, colnames[i] := rowSums(.SD, na.rm = T), .SDcols = grep(paste0("^",i), names(data)) ]    
      
}


# save file to RDS
red.data = data[,.SD, .SDcols = c(1,47:51)]
saveRDS(red.data, "reduced_clc.RDS")


# 3. Cluster --------------------------------------------------------------

# red.data[,.SD,.SDcols = 2:6] %>% 
# kmeans(centers = 3) -> fit
# groupID <- fit$cluster %>% tibble::enframe(name = c("CLUSTER")) %>% .[,2]
# clust1 = red.data[,"Group" := groupID]
# names(clust1)[7] = c("Geo-group")
# fit$centers %>% vegan::rda() %>% biplot


# 04. Add to map  ---------------------------------------------------------

# catchments = st_read("../01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-13_allGRcountires_WGS84_withOV.gpkg")
# three_geo = left_join(catchments, clust1[,c(1,7)], by = "WSO1_ID")
# 
# st_write(obj = three_geo, dsn = "cluster_3_map.gpkg")
