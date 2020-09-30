# -------------------------------------------- #
### --- Discharge per catchment function --- ### 
# -------------------------------------------- #

# GR WP1 
# 15.05.19 
# Wrap the discharge per catchment script in a function 


      ## -- PACKAGES -- ## 
   
      pacman::p_load(here,
                     sf,
                     dplyr,
                     magrittr,
                     raster,
                     data.table
                     )
      
      # read in catchment layer 
      cat = st_read("01_Uni/03_GetReal/01_WP_01/01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-13_allGRcountires_WGS84_withOV.gpkg")

      # subset to WSO1_ID, NEXTDOWID and geom (which is sticky so it does not have to be explicitly mentioned) 
      # This step is not neccesary but reduces load on memory.
      cat2 = cat %>% filter(WSO1_ID %in% ids) 
      
      # load precipitation rasters into list 
      n = length(precipitation)
      vec_prec = lapply(X = c("01_Uni/03_GetReal/01_WP_01/04_Modelling Discharge/01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/1999_12.tif",
      "01_Uni/03_GetReal/01_WP_01/04_Modelling Discharge/01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/2000_01.tif",
      "01_Uni/03_GetReal/01_WP_01/04_Modelling Discharge/01_Data_Raw/02_CHELSA_Precipitation/02_CHELSA_Precipitation/2000_02.tif"
      ), FUN = brick)
      if (n == 3) vec_prec = lapply(X = c(precipitation[[1]], precipitation[[2]],  precipitation[[3]]), FUN = brick)
      if (n == 2) vec_prec = lapply(X = c(precipitation[[1]], precipitation[[2]]),  FUN = brick)
      if (n == 1) vec_prec = lapply(X = precipitation[[1]], FUN = brick)     
      # Extract rasters from list and assign them to prec_crop
      # Parallel processing does not save time here
      for (i in 1:length(vec_prec)) {
         
         assign(paste0("prec_crop",i),
                crop(x = vec_prec[[i]], 
                     y = cat))
         
      }
      
      # mean of all precipitation layers 
      prec_mean2 <- mget(ls()[grepl("crop", x = ls())]) %>% Reduce(f = "+", x = .)/length(vec_prec)
      
      # remove single rasters from memory 
      rm(list = ls()[grepl("crop", x = ls())])
      rm(vec_prec)
      
      # join rasters with catchments
      print(
         paste0(
            "Beginning join of catchment and precipitation at ",
            stringr::str_split(Sys.time(), " ")[[1]][2],
            ". It should take ~ 1:20h"
         )
      )
      # this function extracts all values of raster cells within one polygon (catchment)

       for (i in seq_len(nrow(cat2))) {
            iteration_catchment  = cat2[i,]
            cropped = raster::crop(prec_mean2, iteration_catchment) 
            extracted = as.data.table(raster::extract(
                  x = cropped,
                  y = iteration_catchment,
                  weights = T,
                  normalizeWeights = F
            ))
            # remove NAs 
            na_position = which(is.na(extracted$value))
            extracted = extracted[-na_position,]
            # rescale in case NAs got removed
            if (sum(extracted$weight) != 1) {
                  rescaling_factor = 1/sum(extracted$weight)
                  extracted$weight = extracted$weight * rescaling_factor
            }
            extracted[,"weighted" := value * weight]
            
            cat2[i,"sum_prec"] = sum(extracted$weighted)
            print(i)
      }
      cat2
      
     # cat_ex <- raster::extract(x = prec_mean, y = cat)
      #  Dauer Hochrechnung  ~ 1:25
      #  Dauer gemessen  ~ 1:20
      # sum all precipitation values within one polygon 
      cat2 = cbind(cat, lapply(FUN = sum, X = cat_ex) %>% unlist)
      names(cat2)[3] = "sum_prec"
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
      
      cat_04e <- mutate(cat2,
                        acc_prec  = sum_prec, # this will hol the accumulated precipitation
                        received = 0,         # how many inflows has this cell received 
                        skipped = 0,          # how often was this cell skipped
                        skipped_why1 = 0,     # which catchment caused the skip? 
                        skipped_why2 = 0,     # which catchment caused the skip?
                        skipped_why3 = 0,     # which catchment caused the skip?
                        order.var = 0         # optimal sequence
   )
      
      rm(cat2)
      # table with number of cells that empty into that cell. Which is the contingence
      # table of the NEXTDOWNID column.
      inflow <- tibble("ID" = as.numeric(names(table(cat_04e$NEXTDOWNID))),
                       "inflow" = as.numeric(table(cat_04e$NEXTDOWNID)))
      
      cat_04e <- left_join(cat_04e, inflow, by = c("WSO1_ID" = "ID"))
      naID <- which(is.na(cat_04e$inflow))
      cat_04e$inflow[naID] <- 0 

      # fill order_giver
      print(
         paste(
            "Order Giver wird nun zugeteilt.",
            "Es ist",
            stringr::str_split(Sys.time(), " ")[[1]][2],
            "Der Schritt sollte ca 11 Minuten dauern"
         )
      )

      for (i in 1:nrow(cat_04e)) {

            id.giver <- which(cat_04e$NEXTDOWNID == cat_04e$WSO1_ID[i])
            cat_04e$order_giver[i] <- cat_04e$inflow[id.giver] %>% .^2 %>% sum
      }
      cat_04 <- arrange(cat_04e, inflow, order_giver)
      #cat_04 <- arrange(cat_04e, inflow, order_giver)
      #rm(cat_04e,inflow)
      
      #order.var.counter = 1

# Loop 1 ------------------------------------------------------------------
      
      print(
         paste(
            "Loop 1 startet at",
            stringr::str_split(Sys.time(), " ")[[1]][2],
            "Es sollte etwa 21 Minuten dauern"
         )
      )
      #  1266.50 Sekunden sind gleich 
      #  21 Minuten
      #  1049.66 Sekunden sind gleich
      #  17.5 Minuten 
      system.time(
      for (FROM in 1:nrow(cat_04)) {
            
            # IF1: Does k give to cell in system? 
            # If: yes: that cell's row is TO
            # If: no skip row 
            if (cat_04$NEXTDOWNID[FROM] %in% cat_04$WSO1_ID) {
                  TO <- which(cat_04$WSO1_ID == cat_04$NEXTDOWNID[FROM])
            }else{
                  next()
            }
            
            # IF2 Does FROM receive at all? 
            # If yes: check that he has already received
            # If no:  give on precipitation
            if (cat_04$inflow[FROM] != 0) {
                  
                  # IF2 Yes 
                  
                  # IF 3: Has he received all its inputs?
                  # If yes: give on precipitation
                  # If no: skip for now
                  if (cat_04$received[FROM] == cat_04$inflow[FROM]) {
                        
                        # IF3 Yes
                        # Give precipitation
                        cat_04$acc_prec[TO] <-
                              cat_04$acc_prec[TO] + cat_04$acc_prec[FROM]
                        
                        # change received value in cat_04 and order.var 
                        cat_04$received[TO] <- cat_04$received[TO] + 1
                        #cat_04$order.var[FROM] = order.var.counter
                        #order.var.counter = order.var.counter + 1
                  } else { 
                        
                        # IF 3 NO 
                        
                        # Skip
                        cat_04$skipped[TO] = cat_04$skipped[TO] + 1
                        
                        if (cat_04$skipped[TO] == 1) {cat_04$skipped_why1[TO] = cat_04$WSO1_ID[FROM]}
                        else if (cat_04$skipped[TO] == 2) {cat_04$skipped_why2[TO] = cat_04$WSO1_ID[FROM]}
                        else if (cat_04$skipped[TO] == 3) {cat_04$skipped_why3[TO] = cat_04$WSO1_ID[FROM]}
                  }   
                  
            } else {
                  
                  # IF2 NO
                  #  Give on precipitation
                  cat_04$acc_prec[TO] <-
                        cat_04$acc_prec[TO] + cat_04$acc_prec[FROM]
                  
                  # change received value in cat_04 
                  cat_04$received[TO] <- cat_04$received[TO] + 1
                  #cat_04$order.var[FROM] = order.var.counter
                  #order.var.counter = order.var.counter + 1
            }
         print(paste(FROM,"of", nrow(cat_04)))
      } # END LOOP FROM ROWS OF WATER GIVERS
)
      cat05 <- cat_04 
      #cat051 = cat05 %>% arrange(skipped, inflow) %>% mutate(order.var = NA)
      cat051 = cat05 %>% arrange(skipped, order.var, inflow)
      # which column is "skipped why 1". This makes the function more robust if
      # I should add or delete columns from the skip matrix of a previous matrix
      n_why_cols =  which(colnames(cat051) == "skipped_why1" )
      ## For loop to resolve the rest 
      
      # ran 4318.00 seconds 
      # which is equal to 72 Minutes 
      
      system.time(
      for (hag in 1:1000000) { 
            #this loop should iteratively finish all catchments thus i let it run 
            #and check the results each run
         
            # will help put variables into order 
            #if (hag == 1) order.var = 1
            if (hag == 1) order.var = max(cat051$order.var,na.rm = T) + 1
            # number of filled catchments this run
            succsess = 0
            all = length(which(cat051$skipped != 0)) 
            if (all == 0) break("finished")

            for (i in 1:nrow(cat051)) { #START LOOP i over rows of skip, Row i receives 
                  
                  # Has this row been skipped? Else next 
                  if (cat051$skipped[i] < 1) next()  
                  
                  #extract catchments that did not give 
                  ID_why = cat051[i, n_why_cols:(n_why_cols + 2)] %>%
                        st_drop_geometry() %>% 
                        as.numeric
                  #where is non zero located
                  position = which(ID_why != 0)
                  #remove zeros
                  ID_why0 = ID_why[ID_why != 0]
                  
                  # get rows from cat05
                  for (j in 1:length(ID_why0)) {
                        
                        rows_cat05_FROM = which(cat051$WSO1_ID == ID_why0[j]) 
                        
                        # IF1: Has the giving cell meanwhile received its input?
                        # YES - then give on precipitation
                        # NO - Skip
                        
                        if (cat051$received[rows_cat05_FROM] == cat051$inflow[rows_cat05_FROM]) { 
                              
                              #IF1 YES
                              cat051$acc_prec[i] <- cat051$acc_prec[i] + cat051$acc_prec[rows_cat05_FROM]
                              
                              # adapt received, skipped, successes, order.var 
                              cat051$received[i] <- cat051$received[i] + 1
                              cat051$skipped[i] <- cat051$skipped[i] - 1  
                              cat051[i, (n_why_cols + position[j] - 1)] = 0
                              succsess = succsess + 1
                              cat051$order.var[rows_cat05_FROM] = order.var 
                              order.var = order.var + 1
                        } else next()
                        
                  }
                  
            }   
            
            print(paste("In run", hag,  "we had", succsess, "of", all))
            if (succsess != 0) {  cat051 = cat051 %>% arrange(skipped, inflow) } else {
                  cat051 = cat051[sample(nrow(cat051)),]
            }
            
      }
      )

      st_write(cat051, "../../../../../Desktop/cat051.gpkg")


cat051$order.var[which(is.na(cat051$order.var))] = 0 
