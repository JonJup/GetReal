### -------------------------------------------------------------------- ###
# --- Clean Juliette Tison-Rosebery and Aurelien Jamoneau Diatom data  --- # 
### -------------------------------------------------------------------- ###

# Jonathan Jupke 
# date: 20.11.19

# In this script I create a harmonized spatial data set from the raw data
# provided by Juliette Tison-Rosebery and Aurelien Jamoneau Diatom @ irstea.

# EPSG based on email: Lambert-93 transformation projected system (EPSG2154)

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Clean data
# 03. Taxonomic Cleaning
# 04. Final Touches
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(
        taxize,
        data.table,
        sf,
        dplyr,
        stringr,
        tmap,
        purrr,
        here
)
setwd(here("010_Irstea"))
data4 = readRDS("03_FinalData/05_191212_final_taxon_join_dia_irstea.RDS")
unique_sites = unique(data4$site)
unique_dates = unique(data4$season) #%>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$site, ~ which(unique_sites == .x )),
        map_int(data4$season, ~ which(unique_dates == .x ))
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_IRSTEA")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data5 = data4[n.sample.data, on = "site_id2"]

data6 = data5[, list(
        gr_sample_id,
        original_site_name = site,
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
        abundance,
        pristine,
        x.coord ,
        y.coord,
        EPSG = 2154,
        data.set = "IRSTEA"
        
        
)]
data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/08_", Sys.Date(), "_diatoms_irstea_w_org_names.RDS"))
