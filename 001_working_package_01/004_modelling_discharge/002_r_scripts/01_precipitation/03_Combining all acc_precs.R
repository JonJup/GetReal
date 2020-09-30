########################################
### -- All acc_precs in one file --- ### 
########################################


# 01. Setup ---------------------------------------------------------------

pacman::p_load(data.table,
               sf,
               dplyr,
               magrittr,
               stringr)


setwd(here::here("03_Data_Processed/02_Precipitation/03_Europe_Precipitation/"))
files = fs::dir_ls()


# 02. Prepare table -------------------------------------------------------
# use the first one as base 

geo_file = st_read(files[1])

geo_file %<>% select(WSO1_ID, Winter_1999 = acc_prec)

for (i in 2:length(files)) {
      
      new_file = st_read(files[i]) %>% st_drop_geometry()
      year = files[i] %>% str_extract(pattern = "[0-9]{4}")
      season = str_split(files[i], pattern = "[0-9]{4}", simplify = T) %>%  
            .[,2] %>% 
            str_split("_", simplify = T) %>% 
            .[,1]
      col.name = paste(season,year, sep = "_")      
      new_file %<>% select(WSO1_ID, acc_prec)
      names(new_file) = c("WSO1_ID", col.name)
      geo_file = left_join(geo_file, new_file, by = "WSO1_ID")
}


# 03.Save to file  --------------------------------------------------------

st_write(geo_file, "all_acc_prec.gpkg")
