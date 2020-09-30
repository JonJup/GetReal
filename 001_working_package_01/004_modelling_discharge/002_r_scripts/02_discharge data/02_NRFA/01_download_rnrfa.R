########################################################
### --- Download GB Discharge with RNRFA package --- ### 
########################################################

#install.packages("rnrfa")
pacman::p_load(rnrfa, data.table, lubridate, dplyr, purrr, stringr)

# meta data for all 1580 stations   
allStations <- catalogue()

# number of stations 
n = nrow(allStations)

# check that IDs are unique 
length(unique(allStations$id)) == n

# setwd to save files 
setwd(here::here("01_Data_Raw/04_rnrfa/"))
gfds = safely(gdf)

# next i 515
for (i in 435:nrow(allStations)) {
      
      check.id = allStations$id[i]  
      check.gdf = gfds(check.id)
      if (is.null(check.gdf$result)) next()                   
      check.gdf = check.gdf$result %>% as.data.table()           
      check.gdf[,"date" := ymd(check.gdf$index)]
      check.gdf = na.omit(check.gdf)
      check.gdf = check.gdf[year(date) >= 2000]
      check.gdf = check.gdf[,-1]
      result.i = data.table(
            ID = rep(check.id, nrow(check.gdf)),
            Value = check.gdf$V1,
            Latitude = rep(allStations$`lat-long`$latitude[i], nrow(check.gdf)),
            Longitude = rep(allStations$`lat-long`$longitude[i], nrow(check.gdf)),
            Rivername = rep(allStations$river[i], nrow(check.gdf)),
            Stationname = rep(allStations$name[i], nrow(check.gdf)),
            Year = year(check.gdf$date),
            Month = month(check.gdf$date),
            Day = day(check.gdf$date))
      result.i = result.i[Year >= 2000 & Year <= 2013]
      if(nrow(result.i) == 0) next()
      if (str_detect(allStations$name[i], "/")) {
         
         allStations$name[i] = str_remove(allStations$name[i], "/")
         
      }
      saveRDS(result.i, paste0(check.id,"_",allStations$name[i], ".RDS"))
      print(i)                                           
}

