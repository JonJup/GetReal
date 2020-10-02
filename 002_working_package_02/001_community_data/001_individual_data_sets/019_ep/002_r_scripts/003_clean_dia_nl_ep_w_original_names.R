# -------------------------------------- #
# --- Clean Edwin Peters Diatom data --- # 
# -------------------------------------- #

# Jonathan Jupke 
# date: 26.09.19
# European Map of Diatoms 

# In this script I create a harmonized spatial data set from the raw data
# provided by Edwin Peters.

# EPSG based on projfinder: 28992

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Clean data
# 03. Taxonomy
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(
  taxize,
  data.table,
  sf,
  dplyr,
  stringr,
  lubridate,
  readr,
  purrr,
  lubridate,
  here
)
setwd(here("019_Edwin Peters/"))
data3 = readRDS("03_FinalData/diatoms/05_191209_final_taxon_join_dia_edwin_peters.RDS")

unique_sites = unique(data3$V1)
unique_dates = unique(data3$date)

data3[, c("site_id", "date_id") := list(
  map_int(data3$V1, ~ which(unique_sites == .x )),
  map_int(data3$date, ~ which(unique_dates == .x ))
)]


data3[, site_id2 := case_when(
  nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
  nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
  nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
  nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data3[, date_id2 := case_when(
  nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
  nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
  nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
  nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
  nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data3[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_DIA_edwin_peters")]

data5 = data3[, list(
  gr_sample_id,
  original_site_name = V1,
  date = as.Date(date),
  year,
  season,
  site_id,
  date_id,
  original_name = taxon,
  species,
  genus,
  family,
  order,
  abundance = counts,
  pristine = NA,
  x.coord = xcoord,
  y.coord = ycoord,
  EPSG = 28992,
  data.set = "ediwn_peters_dia"
  
)]
na.coord = data5[is.na(y.coord), unique(gr_sample_id)] 
data5 = data5[!(gr_sample_id %in% na.coord)]
data5  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data5, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_edwin_peters_w_org_names.RDS"))   
