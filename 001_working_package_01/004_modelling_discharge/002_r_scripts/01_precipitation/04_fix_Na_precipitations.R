#############################################
##### ---- FIX NAs IN PRECIPITATION ---- ####
#############################################

# date: 02.09.19 

# Nas occured in some instances of the precipitation calculations, due to NA in a function the results of which were then summed. The bug has been fixed.

# Setup -------------------------------------------------------------------

pacman::p_load(sf, dplyr, data.table, raster)
setwd("~/01_Uni/03_GetReal/01_WP_01/04_Modelling Discharge/")
ccm = st_read("../01_Stream_Network/01_CCM2/03_data_processeed/Catchment/2019-06-13_allGRcountires_WGS84_withOV.gpkg")
ids = c(
        566101,
        1019912,
        1029505,
        1019094,
        1029683,
        1024551,
        1026878,
        1021483,
        1018554,
        1018923,
        1025699,
        1023018,
        1147560,
        1144558,
        1151334,
        1150937,
        1144819
)
monthata = c("winter", "spring","summer","autumn")
ccm2 = ccm %>% filter(WSO1_ID %in% ids) 

for (year_loop in 2000:2013) {
        
        for (season_loop in 1:4) {
        
                if (monthata[season_loop] == "winter") {
                        month =  c("12", "01", "02")
                        year = c(year_loop - 1, year_loop, year_loop)
                } else if (monthata[season_loop] == "spring") {
                        month = c("03", "04", "05")
                        year = rep(year_loop, 3)
                } else if (monthata[season_loop] == "summer") {
                        month =  c("06", "07", "08")
                        year = rep(year_loop, 3)
                } else if (monthata[season_loop] == "autumn") {
                        month =  c("09", "10", "11")
                        year = rep(year_loop, 3)
                }
                
                
        
                precip_names = paste0(
                        "01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/",
                        year,"_",month,".tif")
   
                vec_prec = lapply(X = precip_names, FUN = raster)
                for (i in 1:length(vec_prec)) {
                        
                        assign(paste0("prec_crop",i),
                               raster::crop(
                                       x = vec_prec[[i]], 
                                       y = ccm2
                                       )
                               )
                        
                }
                prec_mean2 <- mget(ls()[grepl("crop", x = ls())]) %>% Reduce(f = "+", x = .)/length(vec_prec)
                prec_mean2[prec_mean2 < 0] <- 0
                # remove single rasters from memory 
                rm(list = ls()[grepl("crop", x = ls())])
                rm(vec_prec)
                for (i in seq_len(nrow(ccm2))) {
                        iteration_catchment  = ccm2[i,]
                        extracted = as.data.table(raster::extract(
                                x = prec_mean2,
                                y = iteration_catchment,
                                weights = T,
                                normalizeWeights = F
                        ))
                        # for some catchments (maybe only WSO1_ID = 566101) extracted is a data.table with zero rows
                        # this results in an error further down if its not taken care of up here. 
                        if (nrow(extracted) == 0) {
                                
                                ccm2[i,"sum_prec"] = NA
                                next()
                                
                        }
                        # remove NAs 
                        na_sum = is.na(extracted$value) %>% sum
                        if (na_sum > 0) {
                                extracted = extracted[which(is.na(extracted$value))]
                        }
                        # rescale in case NAs got removed
                        if (sum(extracted$weight) < 1) {
                                rescaling_factor = 1/sum(extracted$weight)
                                extracted$weight = extracted$weight * rescaling_factor
                        }
                        extracted[,"weighted" := value * weight]
                        
                        ccm2[i,"sum_prec"] = sum(extracted$weighted)
                        print(i)
                }
                
                ### -- load corresponding result file -- ##
                res_file_name = paste0(year[2],monthata[season_loop],"_agg_prec.gpkg")
                res_file = st_read(paste0("03_Data_Processed/02_Precipitation/03_Europe_Precipitation/",  res_file_name)) %>% 
                        st_drop_geometry() %>% 
                        setDT
                res_file[WSO1_ID %in% ids, c("sum_prec", "acc_prec") := list(ccm2$sum_prec, ccm2$sum_prec)]
                save_file_name = stringr::str_remove(res_file_name, ".gpkg$")
                fwrite(x = res_file, file = paste0("03_Data_Processed/02_Precipitation/03_Europe_Precipitation/fixed/", save_file_name, ".csv"))
                print(paste(year_loop, monthata[season_loop], "@", Sys.time()))
}}
