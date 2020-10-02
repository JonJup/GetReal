### ---------------------------------- ### 
# --- Clean Diatom data from Ecosurv --- #
### ---------------------------------- ###

# date: 02.08.19

# In this script I clean the diatom data from Ecosurv

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(readxl,
               magrittr,
               sf,
               taxize,
               data.table,
               stringr,
               tidyr,
               dplyr,
               purrr,
               tmap,
               lubridate,
               here)

setwd(here("009_Hermann van Dam"))
data4 = readRDS("03_FinalData/diatoms/05_191216_final_taxon_join_dia_dia_Ecosurv.RDS")

unique_sites = unique(data4$Site)
unique_dates = unique(data4$date) %>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$Site, ~ which(unique_sites == .x )),
        map_int(data4$date, ~ which(unique_dates == .x ))
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_Ecosurv")]

data6 = data4[, list(
        gr_sample_id,
        original_site_name = Site,
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
        abundance,
        pristine,
        x.coord = y.coord,
        y.coord = x.coord,
        EPSG,
        data.set = "dia_Ecosurv"
        
)]

data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_ecosurv_w_org_names.RDS"))
