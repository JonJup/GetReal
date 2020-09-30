### --- Combine NRFA Stations --- ###

# 10.07.19

# From the Rnrfa/Download Data.R script I obtained 1310 files with stations. Now
# I want one continuous file. The problem will be that this file will be huge!
# The separate RDS files are 21.2 GB in size so maybe I should instead opt for a
# peacemeal appraoch. Combine tables to 10 tables. Check wheather they
# intersect. Then combine endprodcuts


# Setup -------------------------------------------------------------------
setwd(here::here("01_Data_Raw/04_rnrfa/"))
pacman::p_load(sf,dplyr, data.table, tmap)

files = fs::dir_ls()
# seq.int makes only integer sequences 
breakpoints = as.integer(seq(from = 1, to = length(files), length.out = 31))
savenames = paste0("../03_Data_Processed/NFRA/NRFA_discharge",1:30,".RDS")
for (i in 1:30) {
      if (i == 1) last = 1
      if (i != 1) last = breakpoints[i] + 1
      k = breakpoints[i + 1]
      for (j in last:k) {
            
            if (j == last) dat = readRDS(files[k]) %>% setDT()
            if (j != last) {
                  
                  dat.temp = readRDS(files[k]) %>% setDT()
                  dat = rbindlist(list(dat, dat.temp))
            }
                  
      print(paste("j =", j))      
      }
      saveRDS(dat, savenames[i])
      print(paste("i" = i))
}

rm(list = ls())
## Now I reduced the number of files from 1310 to 30.
## These will now sequencially be 
## 1) reduce each the dataset to location data 
## 2) check in which catchment they fall 
## 3) check if they are near a stream of third order 



setwd(here::here("03_Data_Processed/01_Discharge/NRFA"))
files = fs::dir_ls()


for (i in 1:length(files)) {
      
      x = readRDS(files[i])
      setDT(x)
      glimpse(x)
      
      x2 = unique(x, by = "Longitude")
      tmap_mode("view")
      # proj Qornoq EPSG 4324
      x3 = st_as_sf(x2, coords = c("Longitude", "Latitude"), crs = 4287 )
      tm_shape(x3) + tm_dots()
      }