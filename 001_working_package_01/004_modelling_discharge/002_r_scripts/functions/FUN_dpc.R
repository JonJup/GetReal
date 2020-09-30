# -------------------------------------------- #
### --- Discharge per catchment function --- ### 
# -------------------------------------------- #

# GR WP1 
# 16.05.19 
# Wrap the discharge per catchment script in a function 

# -TODO- # 
#i) defensive coding: check for packages 


dpc <- function(catchment, precipitation){
      
      # read in catchment layer 
      cat = st_read(catchment)
      
      # subset to WSO1_ID, NEXTDOWID, order.var and geom (which is sticky so it
      # does not have to be explicitly mentioned) This step is not neccesary but
      # reduces load on memory.
      cat = cat[,c(1,15,46)] 
      vec_prec = lapply(X = c("../../../../04_Modelling Discharge/01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/1999_12.tif",
     "../../../../04_Modelling Discharge/01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/2000_01.tif",
      "../../../../04_Modelling Discharge/01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/2000_02.tif"), FUN = raster)
      # load precipitation rasters into list 
      #prec_mean = stack(precipitation)
      
      n = length(precipitation)
      if (n == 3) vec_prec = lapply(X = c(precipitation[[1]], precipitation[[2]],  precipitation[[3]]), FUN = brick)
      if (n == 2) vec_prec = lapply(X = c(precipitation[[1]], precipitation[[2]]),  FUN = brick)
      if (n == 1) vec_prec = lapply(X = precipitation[[1]], FUN = brick)                             
      
      for (i in 1:length(vec_prec)) {
         assign(paste0("prec_crop",
                       i),
                crop(x = vec_prec[[i]],
                     y = as.vector(st_bbox(cat))))
         
      }
      
      prec_mean = mget(ls()[grepl("crop", x = ls())]) %>% Reduce(f = "+", x = .)/length(vec_prec)
      
      
      # join rasters with catchments
      print(
         paste0(
            "Beginning join of catchment and precipitation at ",
            stringr::str_split(Sys.time(), " ")[[1]][2],
            ". It should take ~ 1:20h"
         )
      )
      # this function extracts all values of raster cells within one polygon (catchment)
      # started 14.06.19 @ 8:19
      for (i in seq_len(nrow(cat))) {
         iteration_catchment  = cat[i,]
         cropped = raster::crop(prec_mean, iteration_catchment) 
         extracted = as.data.table(raster::extract(
            x = cropped,
            y = iteration_catchment,
            weights = T,
            normalizeWeights = F
         ))# remove NAs 
         # remove NAs 
         na_sum = is.na(extracted$value) %>% sum
         if (na_sum > 0) {
               extracted = extracted[which(is.na(extracted$value))]
         }
         extracted[,"weighted" := value * weight]
         
         cat2[i,"sum_prec"] = sum(extracted$weighted)
         print(i)
      }
      
      # remove unnessecary files 
      rm(prec_mean)
      
      
      # Handeling NAs -----------------------------------------------------------
      
      # IF there are NAs in the precipitation data 
      # NAs are replaced by the means of ther neighbouring catchments 
      if (sum(is.na(cat$sum_prec)) != 0) {
         
         # find neighbours 
         sparse_mtx <- st_intersects(cat, cat)
         id <- which(is.na(cat$sum_prec))
         
         # replace NAs with means 
         for (i in 1:length(id)) {
               sparse_mtx_i <- sparse_mtx[[id[i]]]
               sparse_mtx_i_2 <- sparse_mtx_i[-which(sparse_mtx_i == id[i])]
               cat[id[i], ]$sum_prec <-
                     mean(cat[sparse_mtx_i_2, ]$sum_prec, na.rm = T)
         }
      }
      
   # Create downstream accumulation ------------------------------------------
      
      cat3 <- mutate(cat, acc_prec  = sum_prec)
      
      rm(cat)
      cat4 = arrange(cat3, order.var)
      
      
                  # order = st_read("02_Processing/Functions/order_all_countries.gpkg") %>%  
                  #    dplyr::select(WSO1_ID, order.var) %>% 
                  #    st_drop_geometry()
                  # inflow <- tibble("ID" = as.numeric(names(table(cat3$NEXTDOWNID))),
                  #                  "inflow" = as.numeric(table(cat3$NEXTDOWNID)))
                  # cat3e <- left_join(cat3, inflow, by = c("WSO1_ID" = "ID"))
                  # naID <- which(is.na(cat3e$inflow))
                  # cat3e$inflow[naID] <- 0 
                  # 
                  # cat4 = left_join(x = cat3e, y = order)

      
   # Loop 1 ------------------------------------------------------------------
      
      print(
         paste(
            "Loop 1 startet at",
            stringr::str_split(Sys.time(), " ")[[1]][2],
            "Es sollte etwa 21 Minuten dauern"
         )
      )
      for (FROM in 1:nrow(cat4)) {
            
            # IF1: Does k give to cell in system? 
            # If: yes: that cell's row is TO
            # If: no skip row 
            if (cat4$NEXTDOWNID[FROM] %in% cat4$WSO1_ID) {
                  TO <- which(cat4$WSO1_ID == cat4$NEXTDOWNID[FROM])
            }else{
                  next()
            }
            
            # IF2 Does FROM receive at all? 
            # If yes: check that he has already received
            # If no:  give on precipitation
            #if (cat4$inflow[FROM] != 0) {
                  
                  # IF2 Yes 
                  
                  # IF 3: Has he received all its inputs?
                  # If yes: give on precipitation
                  # If no: skip for now
                  #if (cat4$received[FROM] == cat4$inflow[FROM]) {
                        
                        # IF3 Yes
                        # Give precipitation
                        #cat4$acc_prec[TO] <-
                              cat4$acc_prec[TO] + cat4$acc_prec[FROM]
                        
                        # change received value in cat4 and order.var 
                        #cat4$received[TO] <- cat4$received[TO] + 1

                 # } else { 
                        
                        # IF 3 NO 
                        
                        # Skip
                     #   cat4$skipped[TO] = cat4$skipped[TO] + 1
                        
                    #    if (cat4$skipped[TO] == 1) {cat4$skipped_why1[TO] = cat4$WSO1_ID[FROM]}
                    #    else if (cat4$skipped[TO] == 2) {cat4$skipped_why2[TO] = cat4$WSO1_ID[FROM]}
                    #    else if (cat4$skipped[TO] == 3) {cat4$skipped_why3[TO] = cat4$WSO1_ID[FROM]}
                  #}   
                  
           # } else {
                  
                  # IF2 NO
                  #  Give on precipitation
                  cat4$acc_prec[TO] <-
                        cat4$acc_prec[TO] + cat4$acc_prec[FROM]
                  
                  # change received value in cat4 
                  #cat4$received[TO] <- cat4$received[TO] + 1

           # }
         print(paste(FROM,"of", nrow(cat4)))
      } # END LOOP FROM ROWS OF WATER GIVERS
   

   # Return result -----------------------------------------------------------

      
      return(cat4)
      
}

