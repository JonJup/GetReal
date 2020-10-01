### ------------------------------------------ ### 
# --- Clean Diatom data from Miguel Iglesias --- #
### ------------------------------------------ ###

# date: 16.09.19 

# In this script I clean the diatom data from Miguel Iglesias
# EPSG: 32630 -- WGS 84 / UTM zone 30N 

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
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
               tmap,
               purrr,
               here)

setwd(here("002_Miguel_Iglesias/"))

data4 = readRDS("03_FinalData/diatoms/05_191217_final_taxon_join_dia_Miguel_Iglesias.RDS")

# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$site)
unique_dates = unique(data4$date) %>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$site, ~ which(unique_sites == .x )),
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_MiguelIglesias")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data5 = data4[n.sample.data, on = "site_id2"]

data6 = data5[, list(
        gr_sample_id,
        original_site_name = site,
        date,
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
        pristine = prisitne,
        x.coord,
        y.coord = y.yoord,
        EPSG,
        data.set = "dia_Miguel_Iglesias"
        
)]
data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_MI_w_org_names.RDS"))   


