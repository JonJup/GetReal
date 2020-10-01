# -------------------------------------------------------- #
### --- Extract original names form low_impact sites --- ### 
### ---------------------- Diatoms --------------------- ###
### ------------------- Janne Soininen ----------------- ###
# -------------------------------------------------------- #

#date: 03.04.20
#Jonathan Jupke 

# 01. Setup -------------------------------------------------------------------

pacman::p_load(dplyr, magrittr, sf, stringr, data.table)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/006_Janne_Soininen/")


# read data ---------------------------------------------------------------

data     <- readRDS("03_FinalData/01_191216_data_before_taxon_clean_Janne_Soininen.RDS")
low_impact <- readRDS("../100_Combined/001_Diatoms/003_processed_data/004_2020-04-09_dia_data_low_ls_all.RDS")

# Cleaning ----------------------------------------------------------------

setDT(data)
setDT(low_impact)

# Extract names  ----------------------------------------------------------

low_impact <- low_impact[data.set == "Janne_Soininen"]
data_new <- data[Site %in% low_impact$original_site_name]
data_new %<>% select(taxon, Site)
TU <- sort(unique(data_new$taxon))

# save to file  -----------------------------------------------------------

setwd("../100_Combined/001_Diatoms/orignal_names_low_impact_sites/")

saveRDS(data_new,
        paste0("005_",
               Sys.Date(),
               "_data_from_low_impact_sites_JS.RDS"
        ))
