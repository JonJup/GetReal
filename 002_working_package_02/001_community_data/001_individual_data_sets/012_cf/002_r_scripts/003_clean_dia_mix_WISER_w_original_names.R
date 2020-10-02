#########################################
### --- Clean DIA data from WISER --- ### 
#########################################

# 09.10.19

# GR WP 02 
# EPSG = 4326

# Setup -------------------------------------------------------------------
pacman::p_load(dplyr, readxl, stringr, readr, data.table, taxize, sf, tmap, purrr, here)
setwd(here("012_Christian Feld"))

data4 = readRDS("03_FinalData/diatoms/05_191212_final_taxon_join_dia_dia_WISER.RDS")

unique_sites = unique(data4$site_id)

data4[, c("site_id") := list(
  map_int(data4$site_id, ~ which(unique_sites == .x ))
)]


data4[, site_id2 := case_when(
  nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
  nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
  nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
  nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_dia_WISER")]

data5 = data4

data6 = data5[, list(
  gr_sample_id,
  original_site_name = site_name,
  date = as.Date(NA),
  year = NA,
  season = NA,
  site_id,
  date_id = NA,
  original_name = taxon,
  species,
  genus,
  family,
  order,
  abundance = NA,
  pristine = NA,
  x.coord = X.Coord,
  y.coord = Y.Coord,
  EPSG = 4258,
  data.set = "dia_Wiser"
  
)]
data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_WISER_w_org_names.RDS"))   

