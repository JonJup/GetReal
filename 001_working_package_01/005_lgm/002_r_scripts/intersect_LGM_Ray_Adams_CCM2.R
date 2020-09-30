##########################################################
#### ---- State at last Galcial Maximum in CCM2 ---- #####
##########################################################

# date: 27.08.19


# Setup -------------------------------------------------------------------

pacman::p_load(sf,dplyr,purrr, data.table)

ccm = st_read("01_Uni/03_GetReal/01_WP_01/01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")
lgm = st_read("01_Uni/03_GetReal/01_WP_01/05_Glaciers/01_original_data/state_at_LGM_Ray_and_Adams_2001/world_cont.shp")

# I do not know if lgm is truly WGS84 but I checked the file visually in QGIS and it looks fine. 
lgm2 = st_set_crs(lgm, 4326) %>% 
        st_crop(ccm)

#st_write(lgm2, "01_Uni/03_GetReal/01_WP_01/05_Glaciers/03_processed_data/test.gpkg")        

# build new 
outputtable = data.table("WSO1_ID" = rep(0,nrow(ccm)),
                          "veg_id" = rep(0,nrow(ccm)))
                  
                         

options(warn = -1)
safe_st_intersection = safely(st_intersection)
beepr::beep_on_error(
        for (i in 1:nrow(ccm)) {
                
                # extract row
                cat = ccm[i, ]
                # fill output table ID
                outputtable[i,"WSO1_ID"] = cat$WSO1_ID
                # calculate area 
                #area_cat = st_area(cat)
                # intersection
                cat_int_s = safe_st_intersection(cat, lgm2)
                
                if (!(is.null(cat_int_s$error))) {
                        cat_int_pre <- st_intersects(cat, lgm2)
                        cat_int = lgm2[cat_int_pre[[1]],]
                        cat_int_s = safe_st_intersection(cat, cat_int)
                        if (!(is.null(cat_int_s$error))) {
                                cat2 = st_buffer(cat, dist = 0)
                                cat_int2 = st_buffer(cat_int, dist = 0)
                                cat_int_s = safe_st_intersection(cat, cat_int2)
                                if (!(is.null(cat_int_s))) {
                                        cat_int_s =    safe_st_intersection(cat2, cat_int2)
                                }   
                        }
                }
                # how many different geologies intersect? 
                cat_int = cat_int_s$result
                ngeo = nrow(cat_int)
                if (ngeo == 1) {
                        outputtable[i, veg_id := cat_int$VEG_ID] 
                        
                }
                if (ngeo > 1) {
                        # calculate relative areas
                        frac_area = st_area(cat_int)
                        # select greater area 
                        cat_int2 = cat_int[which(frac_area == max(frac_area)),]
                        #attributes(frac_area) = NULL
                        outputtable[i, veg_id := cat_int2$VEG_ID] 

                }
                print(i)
        }        
) 

saveRDS(outputtable, "LGM_WSO1ID.RDS")

