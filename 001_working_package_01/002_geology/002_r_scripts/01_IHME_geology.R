################################
#### ---- IHME to CCM2 ---- ####
################################

# date: 23.08.19

# In this script I will intersect IHME v11 (obtained from BOKU via Florian
# Borgwardt) with CCM2 to add groudwater geological information.


# Setup -------------------------------------------------------------------

# load libraries
pacman::p_load(sf, data.table,dplyr, purrr)

# read in data 
ihme = st_read("IHME/01_rawdata/Florian Borgwardt/IHME1500_v11/ihme_1500_litho4changed.shp")
ccm = st_read("../01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")


# transform ihme 
ihme2 = st_transform(ihme, crs = st_crs(ccm))

# build new 
# outputtable = data.table("WSO1_ID" = rep(0,nrow(ccm)), 
#                          "siliceous" = rep(0,nrow(ccm)), 
#                          "sediments" = rep(0,nrow(ccm)), 
#                          "calcareous" = rep(0,nrow(ccm)))

# load started version 
outputtable = readRDS("55550_qs_geology.RDS")
# Intersection ------------------------------------------------------------
options(warn = -1)
safe_st_intersection = safely(st_intersection)
beepr::beep_on_error(
for (i in 284967:nrow(ccm)) {
        
        # extract row; buffer to prevent intersection error that occurs with self inersecting polygons
        cat = ccm[i, ]
        # fill output table ID
        outputtable[i,"WSO1_ID"] = cat$WSO1_ID
        # calculate area 
        area_cat = st_area(cat)
        # intersection
        cat_int_s = safe_st_intersection(cat, ihme2)
        if (!(is.null(cat_int_s$error))) {
                cat_int_pre <- st_intersects(cat, ihme2)
                cat_int = ihme2[cat_int_pre[[1]],]
                cat_int_s = safe_st_intersection(cat, cat_int)
                if (!(is.null(cat_int_s$error))) {
                        cat2 = st_buffer(cat, dist = 0)
                        cat_int2 = st_buffer(cat_int, dist = 0)
                        cat_int_s = safe_st_intersection(cat2, cat_int2)
                           
                }
        }
        # how many different geologies intersect? 
        cat_int = cat_int_s$result
        ngeo = nrow(cat_int)
        if (ngeo == 1) {
                material = as.character(cat_int$acid_basic[1])
                outputtable[i, (material) := 100 ] 
                
                }
        if (ngeo > 1) {
                # calculate relative areas
                frac_area = st_area(cat_int) / area_cat
                attributes(frac_area) = NULL
                for (j in 1:ngeo) {
                        material = as.character(cat_int$acid_basic[j])
                        plusarea =  frac_area[j] * 100
                        oldarea = outputtable[i, (get(material))]
                        attributes(plusarea) = NULL
                        outputtable[i, (material) := plusarea + oldarea]
                }
        }
        
        print(cat(i, "\n", "finished @", Sys.time() %>% stringr::str_subset("\ "), "\n",
                  "runtime:", Sys.time() - end.last))
        end.last = Sys.time()
}        
) 
        
#quick save 
# saveRDS(outputtable, "284965_qs_geology.RDS")

#final save 
saveRDS(outputtable, "final_ihme_geology.RDS")


# Final data cleaning -----------------------------------------------------
outputtable2 = outputtable
for (i in 1:nrow(outputtable)) {
        if (sum(outputtable[i, 2:4]) != 100) {
                multiplication_factor = 100/sum(outputtable[i, 2:4])
                outputtable2[i, 2:4] = round(outputtable[i,2:4] * multiplication_factor)
        }
        print(i)
}               
#final save 
saveRDS(outputtable2, "final_ihme_geology.RDS")

