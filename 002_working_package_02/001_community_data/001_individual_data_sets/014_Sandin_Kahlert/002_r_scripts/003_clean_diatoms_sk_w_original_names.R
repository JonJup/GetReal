### ---------------------------------- ### 
# --- Clean Diatoms Sandin & Kahlert --- #
### ---------------------------------- ### 

## 21.08.19
## GR WP2 
## Here I clean and harmonize the diatom data provided by Maria Kahlert and Leonard Sandin

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Clean Data 
# 03. Taxonomic Cleaning 
# 04. Final Touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate)
setwd(here::here("014_Sandin_Kahlert/"))
sites = readxl::read_excel("01_OriginalData/AQEM_diatoms_coordinates.xlsx") %>% setDT
data4 = readRDS("03_FinalData/diatoms/05_191211_final_taxon_join_dia_dia_Leonard_Sandin.RDS")

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


data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_dia_Leonard_Sandin")]

data5 = data4

data5 = data5[sites, on = "Site"]
names(data5)[14:15] <- c("y.coord", "x.coord")
data6 = data5[, list(
        gr_sample_id,
        original_site_name = Site,
        date = as.Date(NA),
        year = 2000,
        season = NA,
        site_id,
        date_id = NA,
        original_name = taxon,
        species,
        genus,
        family,
        order,
        abundance = Abundance,
        pristine = NA,
        x.coord,
        y.coord,
        EPSG = 2400,
        data.set = "dia_Leonard_Sandin"
        
)]
data6 <- data6[!is.na(x.coord)]

data6  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data6, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_sandin_kahlert_w_org_names.RDS"))   
