# ---------------------------------------------- #
### --- Calculate Aggregated precipitation --- ###
# ---------------------------------------------- #


# 01. Setup ---------------------------------------------------------------
# load packages
pacman::p_load(sf,
               dplyr,
               raster,
               data.table,
               here,
               stringr
)

# load function
source(here::here("/02_Processing/Functions/FUN_dpc.R"))

files = fs::dir_ls(here("01_Data_Raw", "02_CHELSA_Precipitation", "02_CHELSA_Precipitation/"))
n.files = length(files)
lookup = data.frame(i = seq_len(n.files),
                    year = "NA",
                    month = "NA",
                    name = "NA",
                    stringsAsFactors = F) 

#create lookup table 
for (k in seq_len(n.files)) {

   dat = files[k]
   date = str_split(dat, "/") %>% 
      unlist %>% 
      .[length(.)] %>% 
      str_split("\\.") %>% 
      unlist %>% 
      .[1]
   year = str_extract(date, pattern = regex("[0-9]{4}"))
   month = str_split(date, "_") %>% 
      unlist %>% 
      .[2]
   name = paste0("AggPrec", year,"_",month)
   lookup[k,2] = year
   lookup[k,3] = month
   lookup[k,4] = name
}


# spring
look.spring = lookup %>% filter(month %in% c("03","04","05") & year %in% c(2000:2013))
loop_over = seq(from = 1, to = nrow(look.spring), by = 3)

# summer 
look.summer = lookup %>% filter(month %in% c("06","07","08") & year %in% c(2000:2013))
loop_over = seq(from = 1, to = nrow(look.summer), by = 3)

#autumn
look.autumn = lookup %>% filter(month %in% c("09","10","11") & year %in% c(2000:2013))
loop_over = seq(from = 1, to = nrow(look.autumn), by = 3)[12:14]

#winter
look.winter = lookup %>% filter(month %in% c("12","01","02") & year %in% c(1999:2013))
look.winter = look.winter[-c(1:2),]
loop_over = seq(from = 1, to = nrow(look.winter), by = 3)[1:15]

# here ran 4 ,6:12
# here runs 11:12
for (k in loop_over[14]) {
     
   save.name =  paste0(look.winter[k,"year"],"winter_agg_prec")
   assign(save.name,
          dpc(
             catchment = "../01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-06-13_allGRcountires_WGS84_withOV.gpkg",
             precipitation = list(
                files[look.winter[k, "i"]],
                files[look.winter[k + 1, "i"]],
                files[look.winter[k + 2, "i"]]
                )
          )
          
          
          )
      st_write(obj = get(save.name),
               dsn = paste0("03_Data_Processed/02_Precipitation/03_Europe_Precipitation/",save.name,".gpkg"))
}

