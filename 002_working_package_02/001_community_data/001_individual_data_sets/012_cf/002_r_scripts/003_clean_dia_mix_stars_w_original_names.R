#########################################
# --- Clean DIA data from STARS --- # 
#########################################

# 08.10.19

# GR WP 02 
# EPSG = 4326



# 01. Setup -------------------------------------------------------------------
pacman::p_load(dplyr, readxl, stringr, readr, data.table, taxize, sf, purrr, tmap,here)
setwd(here("012_Christian Feld"))
data4 = readRDS("03_FinalData/05_191212_final_taxon_join_dia_dia_STAR.RDS")

unique_sites = unique(data4$site_id) %>%  sort
unique_dates = unique(data4$season) %>% sort

data4[, c("site_id", "date_id") := list(
  unlist(map(data4$site_id, ~ which(unique_sites == .x ))),
  unlist(map(data4$season, function(x) { if (is.na(x)) {NA} else {which(unique_dates == x)} }))
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_STAR")]

data6 = data4[, list(
  gr_sample_id,
  original_site_name = `site name`,
  date = as.Date(NA),
  year = NA,
  season,
  site_id,
  date_id,
  original_name = taxon,
  species,
  genus,
  family,
  order,
  abundance,
  pristine = ifelse(prot_pristine == "good", 1, 0),
  x.coord = Longitude,
  y.coord = Latitude,
  EPSG = 4326,
  data.set = "dia_Star"
  
)]

data6 = data6[!(is.na(x.coord))]

data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_stars_w_org_names.RDS"))   

