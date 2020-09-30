##################################################
#### ---- Prediction of discharge values ---- ####
##################################################

# 12.08.19
# In this script I predict the discharge values for all catchments 


# Setup -------------------------------------------------------------------

pacman::p_load(sf,
               data.table,
               dplyr,
               magrittr,
               plyr)


setwd(here::here())

# IO 
      # layer with all aggregated precipitations 
      prec = st_read(dsn = "03_Data_Processed/02_Precipitation/03_Europe_Precipitation/all_acc_prec.gpkg") %>% 
            st_drop_geometry() %>% 
            setDT
      # dicharge ID to WSO1_ID 
      IDtoID = readRDS("03_Data_Processed/01_Discharge/combined/Stations_ID_to_WSO1ID.RDS") %>% st_drop_geometry() %>% setDT
      # discharge data
      discharge = readRDS("03_Data_Processed/01_Discharge/combined/discharge_combined_and_filtered.RDS") %>% 
            setDT()
      #ccm2 
      ccm = st_read("../01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-05_allGRcountires_WGS84.gpkg") %>% 
            st_drop_geometry() %>% 
            setDT
      beepr::beep()
# Cleaning  ---------------------------------------------------------------

# remove large rivers 
      largerivers = discharge[discharge > 1000, unique(river)]
      discharge2 = discharge[!(river %in% largerivers)]
      # joins 
      discharge.join = discharge2[IDtoID, on = "ID"]
      names(prec)[2] = "winter_1999"
      ccm.2 = ccm[IDtoID, on = "WSO1_ID"]
      
      # original 
      all_years = 2000:2013
      # altered version for restart 
      # all_years = 2006:2013
      all_month = 1:12
   
             
      # year loop --
      for (year_looper in 1:length(all_years)) {

                 year = all_years[year_looper]
                    
                  # month loop --
                  for (month_looper in 1:12) {

                     month = all_month[month_looper]
                     # this list gets all the daily entries 
                     month_list <- list()
                     
                     
                     if (month %in% c(1,3,5,7,8,10,12)) all_day = 1:31
                     if (month %in% c(2)) {
                        # leapyears 
                        if (year %% 4 == 0) all_day = 1:29
                        if (year %% 4 != 0) all_day = 1:28
                     }
                     if (month %in% c(4,6,9,11)) all_day = 1:30
                     
                     season = ifelse(month %in% c(12,1,2), "winter",
                                     ifelse(month %in% c(3,4,5), "spring",
                                            ifelse(month %in% c(6,7,8), "summer", "autumn")))
                     
                     # day loop -- 
                     for (day_looper in all_day) {
                        day = all_day[day_looper]
                     
                        # subset discharge data to date 
                        dis.join.sub = discharge.join[
                           Year == year & 
                           Month == month &
                           Day == day]
                        
                        # create save name 
                        date_name = paste0("dis", year, "-", month, "-", day)
                        date = paste(year,month,day,sep = "-")
                        # set predictor 
                        exp.var = paste0(season,"_",year)
                        
                        # subset predictor table 
                        prec.sub = prec[,c("WSO1_ID", exp.var), with = F]
                        
                        # join subsets of discharge and predictor and omit NAs
                        dis.join = prec.sub[dis.join.sub, on = "WSO1_ID"] %>% na.omit()
                        
                        # formulate model 
                        fmla = as.formula(discharge ~ get(exp.var))
                        mod = lm(fmla, data = dis.join)
                        
                        
                        # column bind predictions to WSO1ID 
                        assign(
                          date_name,
                          cbind(
                             prec.sub[,1],
                             predict.lm(
                                object = mod,
                                newdata = prec.sub))
                          )
                        # rename column 
                        w = c("WSO1_ID", date)
                        eval(substitute(names(x) <- w, list(x = as.symbol(date_name))))
                        month_list[[day_looper]] = get(date_name)
                        rm(list = c(date_name))
                        print(date_name)
                     } # END day loop 
                     
                     assign(
                        paste(year,month,sep = "_"),
                        join_all(month_list, by = 'WSO1_ID', type = 'left')
                     ) %>% saveRDS(paste0("03_Data_Processed/04_Modelled_Discharge/02_Predicted_Discharge/",year,"_",month,".RDS"))
                     rm(list = paste(year,month,sep = "_"))
                     
                     }
                  
                  
            }

   
            
      
                  