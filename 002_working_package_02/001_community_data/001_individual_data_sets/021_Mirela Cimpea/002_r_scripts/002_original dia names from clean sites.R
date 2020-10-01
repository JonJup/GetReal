### --------------------------------- ###
# --- Clean Diatom data from Landau --- # 
### --------------------------------- ###

# 16.03.20

# In this script I clean the diatom data from landau.

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------

pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, purrr, tmap)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/021_Mirela Cimpea//")

# Cleaning ----------------------------------------------------------------

data     <- readRDS("03_FinalData/01_200316_dia_before_taxon_clean_mirela_cimpean.RDS")
data_all <- readRDS("../100_Combined/001_Diatoms/003_processed_data/2020-03-16_dia_data_low_impact.RDS")
data_all$original_site_name <- as.character(data_all$original_site_name)
data$site <- as.character(data$site)

setDT(data)
data
unique(data_all$data.set)
data_all <- data_all[data.set == "mirella_cimpea"]
data_new <- data[site %in% data_all$original_site_name]
TU <- sort(unique(data_new$taxon))

setwd("03_FinalData/")

saveRDS(TU,
        paste0(
                Sys.Date(),
                "07_original_names_from_low_impact_sites_EP.RDS"
        ))
