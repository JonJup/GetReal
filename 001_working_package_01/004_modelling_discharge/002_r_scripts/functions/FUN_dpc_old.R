# -------------------------------------------- #
### --- Discharge per catchment function --- ### 
# -------------------------------------------- #

# GR WP1 
# 16.05.19 
# Wrap the discharge per catchment script in a function 

# -TODO- # 
#i) defensive coding: check for packages 


library(raster)
library(sf)
precipitation = "../../test/"


dpc <- function(catchment, precipitation){
      
      # read in catchment layer 
      cat = st_read(catchment)
      
      # subset to WSO1_ID, NEXTDOWID and geom (which is sticky so it does not have to be explicitly mentioned) 
      # This step is not neccesary but reduces load on memory.
      cat = cat[,c(1,15)]
      
      # load precipitation rasters into list 
      prec_mean = stack(precipitation)
      # join rasters with catchments
      print(
         paste0(
            "Beginning join of catchment and precipitation at ",
            stringr::str_split(Sys.time(), " ")[[1]][2],
            ". It should take ~ "
         )
      )
      # this function extracts all values of raster cells within one polygon (catchment)

      for (i in seq_len(nrow(cat))) {
            iteration_catchment  = cat[i,]
            cropped = raster::crop(prec_mean2, iteration_catchment) 
            extracted = as.data.table(raster::extract(
                  x = cropped,
                  y = iteration_catchment,
                  weights = T,
                  normalizeWeights = F
            ))
            extracted[,"weighted" := value * weight]
            cat[i,sum_prec] = sum(extracted$weighted)
            print(i)
      }

      ## -- ohne weights -- ## 
      #  Dauer Hochrechnung  ~ 1:25
      #  Dauer gemessen  ~ 1:20
      
      ## -- mit weights -- ## 
      # not finished after eight hours! with weights 
      
   
      # sum all precipitation values within one polygon 
      cat2 = cbind(cat, lapply(FUN = sum, X = cat_ex) %>% unlist)
      names(cat2)[3] = "sum_prec"
      
      # remove unnessecary files 
      rm(cat_ex,cat, prec_mean)
      
      
      # Handeling NAs -----------------------------------------------------------
      
      # IF there are NAs in the precipitation data 
      # NAs are replaced by the means of ther neighbouring catchments 
      if (sum(is.na(cat2$sum_prec)) != 0) {
         
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
      
      cat3 <- mutate(cat2,
                        acc_prec  = sum_prec, # this will hol the accumulated precipitation
       )
      
      rm(cat2)
      order = st_read("02_Processing/Functions/order_all_countries.gpkg") %>%  
         dplyr::select(WSO1_ID, order.var) %>% 
         st_drop_geometry()
      inflow <- tibble("ID" = as.numeric(names(table(cat3$NEXTDOWNID))),
                       "inflow" = as.numeric(table(cat3$NEXTDOWNID)))
      cat3e <- left_join(cat3, inflow, by = c("WSO1_ID" = "ID"))
      naID <- which(is.na(cat3e$inflow))
      cat3e$inflow[naID] <- 0 
      
      cat4 = left_join(x = cat3e, y = order)
      rm(cat3, cat3e, order)
      cat4 = arrange(cat4, order.var)
      
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