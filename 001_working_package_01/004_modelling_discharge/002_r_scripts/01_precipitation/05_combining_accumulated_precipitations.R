########################################
### -- All acc_precs in one file --- ### 
########################################

# date: 03.09.19

# 01. Setup ---------------------------------------------------------------

pacman::p_load(data.table,
               sf,
               dplyr,
               magrittr,
               stringr)


setwd(here::here("03_Data_Processed/02_Precipitation/03_Europe_Precipitation/fixed"))
files = fs::dir_ls()


# 02. Prepare table -------------------------------------------------------
# use the first one as base 

geo_file = fread(files[1])

geo_file = geo_file[, list(WSO1_ID, autumn_2000 = acc_prec)]

for (i in 2:length(files)) {
      
      new_file = fread(files[i])
      year = files[i] %>% str_extract(pattern = "[0-9]{4}")
      season = str_split(files[i], pattern = "[0-9]{4}", simplify = T) %>%  
            .[,2] %>% 
            str_split("_", simplify = T) %>% 
            .[,1]
      col.name = paste(season,year, sep = "_")      
      new_file = new_file[,list(WSO1_ID, acc_prec)] 
      names(new_file) = c("WSO1_ID", col.name)
      geo_file = geo_file[new_file, on = "WSO1_ID"]
      }


# 03.Save to file  --------------------------------------------------------

fwrite(geo_file, "precipitation_all_seasons_nafix.csv")
