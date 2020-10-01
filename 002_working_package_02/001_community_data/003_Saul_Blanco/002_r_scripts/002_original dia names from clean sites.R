# -------------------------------------------------------- #
### --- Extract original names form low_impact sites --- ### 
### ---------------------- Diatoms --------------------- ###
### ------------------- Saul_Blanco -------------------- ###
# -------------------------------------------------------- #

#date: 03.04.20
#Jonathan Jupke 

# 01. Setup -------------------------------------------------------------------

pacman::p_load(dplyr, magrittr, sf, stringr, data.table, purrr)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/003_Saul_Blanco/")

# read data ----------------------------------------------------------------

data       <- readRDS("03_FinalData/01_191217_data_before_taxon_clean_Saul_Blanco.RDS")
low_impact <- readRDS("../100_Combined/001_Diatoms/003_processed_data/004_2020-04-09_dia_data_low_ls_all.RDS")

# clean data  -------------------------------------------------------------

setDT(data)
setDT(low_impact)

data <- data[ !is.na(x.coord)]
data[,site := paste0(y.coord,x.coord)]
data[,date_surrogate := paste0(year, season)]
unique_sites = unique(data$site)
unique_dates = unique(data$date_surrogate) [c(1,2,4,3,6,5,7,8)]

data[, c("site_id", "date_id") := list(
        map_int(data$site, ~ which(unique_sites == .x )),
        map_int(data$date_surrogate, ~ which(unique_dates == .x ))
)]

# extract sites -----------------------------------------------------------

low_impact <- low_impact[data.set == "dia_Saul_Blanco"]
data_new <- data[site_id %in% low_impact$site_id]
data_new %<>% select(taxon, site_id, date_id)
TU <- sort(unique(data_new$taxon))

# save to file  -----------------------------------------------------------

setwd("../100_Combined/001_Diatoms/orignal_names_low_impact_sites/")

saveRDS(data_new,
        paste0("003_",
               Sys.Date(),
               "_data_from_low_impact_sites_SB.RDS"
        ))

             
