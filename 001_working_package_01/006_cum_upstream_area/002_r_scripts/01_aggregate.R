        # ----------------------------------- #
### --- Aggregate Precipitation --- ### 
# ----------------------------------- #

# GR WP1 
# 06.01.20
# Aggregate catchment-level precipitation 


## -- OVERVIEW -- ## 
## -------------- ##

## -- PACKAGES -- ## 

pacman::p_load(
        sf,
        dplyr,
        magrittr,
        raster,
        data.table,
        intrval,
        stringr
)
setwd(here::here())

ccm <- st_read("2019-06-13_allGRcountires_WGS84_withOV.gpkg")

        
setDT(ccm)
ccm = ccm[, geom := NULL]
# sort by order.var 
setorderv(ccm, "order.var")[]
# add accumulated_precipitation column
ccm[, acc_area := AREA_KM2]
        
        # first run 
        
for (FROM in seq_along(rownames(ccm))) {
        
        TO <- which(ccm$WSO1_ID == ccm$NEXTDOWNID[FROM])
        if (length(TO) == 0) next()
        ccm[TO, acc_area :=  acc_area + ccm[FROM, acc_area]]
        if (FROM %% 1000 == 0) print(paste(FROM))
        
}   

ccm <- ccm[, .(WSO1_ID, acc_area)]

saveRDS(ccm, "../../../../07_cum_upstream_area/003_data_processed/cumulative_upstream_area.RDS")                   





