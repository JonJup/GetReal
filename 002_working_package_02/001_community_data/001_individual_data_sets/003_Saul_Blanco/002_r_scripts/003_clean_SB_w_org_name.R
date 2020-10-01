### -------------------------------------- ### 
# --- Clean Diatom data from Saul Blanco --- #
### -------------------------------------- ###

# date: 10.09.19 

# In this script I clean the diatom data from Miguel Iglesias
# EPSG = 4055

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
# libraries
pacman::p_load(here, 
               dplyr, 
               taxize, 
               magrittr, 
               sf, 
               stringr, 
               data.table, 
               lubridate, 
               tidyr,
               readxl,
               purrr,
               tmap,
               here)

# working director  
setwd(here("003_Saul_Blanco"))


data4 = readRDS("03_FinalData/05_191217_final_taxon_join_Saul_Blanco.RDS")
data4 = data4[!is.na(x.coord)]
data4[,site := paste0(y.coord,x.coord)]
data4[,date_surrogate := paste0(year, season)]


unique_sites = unique(data4$site)
unique_dates = unique(data4$date_surrogate) [c(1,2,4,3,6,5,7,8)]

data4[, c("site_id", "date_id") := list(
  map_int(data4$site, ~ which(unique_sites == .x )),
  map_int(data4$date_surrogate, ~ which(unique_dates == .x ))
)]


data4[, site_id2 := case_when(
  nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
  nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
  nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
  nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[, date_id2 := case_when(
  nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
  nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
  nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
  nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
  nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_Saul_Blanco")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data5 = data4[n.sample.data, on = "site_id2"]

data6 = data5[, list(
  gr_sample_id,
  original_site_name = NA,
  date = as.Date(NA),
  year,
  season,
  site_id,
  date_id,
  original_name = taxon,
  species,
  genus,
  family,
  order,
  abundance = NA,
  pristine,
  x.coord,
  y.coord,
  EPSG = 4055,
  data.set = "dia_Saul_Blanco"
)]
data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/08_", Sys.Date(), "_diatoms_sb_w_org_names.RDS"))  
