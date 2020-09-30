# ---------------------------------# 
### --- Download CHELSA data --- ###
# ---------------------------------#

# 05.06.19
# Dowload CHELSA precipitation data 

pacman::p_load(here, dplyr, stringr)
setwd(here("01_Data_Raw","02_CHELSA_Precipitation"))
months = append(paste0(0,1:9), as.character(c(10,11,12)))
all_files = paste0(rep(1979:2013,each = 12),"_",months,".tif")
rest_files = setdiff(all_files, fs::dir_ls())
# precipitation URL 

for (i in seq_len(length(rest_files))) {
      
      # Extracting year and month form loop for file name 
      year = strsplit(rest_files[i], split = "_")[[1]][1]
      
      # subsetting within the pipe did not work for some reason. So I split it up in two steps 
      month_temp = str_split(rest_files[i], pattern = "_")[[1]][2] %>% 
         str_split(pattern = "\\.")
      month = month_temp[[1]][1]
      
      
      layer_url <- paste0("https://www.wsl.ch/lud/chelsa/data/timeseries/prec/CHELSA_prec_",year,"_",month,"_V1.2.1.tif")
      download.file(layer_url, 
                    paste0(year,"_",month,".tif"), 
                    mode = "wb")
      
}


