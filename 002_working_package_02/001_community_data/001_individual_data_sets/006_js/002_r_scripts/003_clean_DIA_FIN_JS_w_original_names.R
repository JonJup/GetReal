### ----------------------------------------- ### 
# --- Clean Diatom data from Janne Soininen --- #
### ----------------------------------------- ###

## 02.08.19

# In this script I clean the diatom data from Janne Soininen

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
# libraries 
pacman::p_load(dplyr,
               stringr, 
               readxl, 
               sf, 
               tidyr, 
               data.table, 
               taxize, 
               magrittr, 
               lubridate, 
               purrr,
               tmap, 
               here)

setwd(here("006_Janne_Soininen/"))

data4 = readRDS("03_FinalData/05_191216_final_taxon_join_Janne_Soininen.RDS")
unique_sites = unique(data4$Site)

data4[, c("site_id") := list(
        map_int(data4$Site, ~ which(unique_sites == .x ))
)]


data4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_Janne_Soininen")]

data5 = data4



for (i in seq_along(colnames(data4))) {
        x = pull(data4[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data4)[i])
        
}

# all sampling was conducted in August 2001 or 2004. See Soininen (2008): The
# Ecological Characteristics of Idiosyncratic and Nested Diatoms
data6 = data5[, list(
        gr_sample_id,
        original_site_name = Site,
        date = as.Date(NA),
        year = NA ,
        season = "summer",
        site_id,
        date_id = NA,
        original_name = taxon,
        species,
        genus,
        family,
        order,
        abundance,
        pristine,
        x.coord = longitude,
        y.coord = latitude,
        EPSG = 2393,
        data.set = "Janne_Soininen"
        
)]

data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/08_", Sys.Date(), "_diatoms_janne_soininen_w_org_names.RDS")) 
